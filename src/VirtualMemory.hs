{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

module VirtualMemory (AccessType(..), translate) where
import Program
import Utility
import qualified CSRField as Field
import Data.Bits
import Prelude
import Data.Int

data VirtualMemoryMode = None | Sv32 | Sv39 | Sv48 deriving (Eq, Show)

getMode :: (Integral s) => s -> VirtualMemoryMode
getMode 0 = None
getMode 1 = Sv32
getMode 8 = Sv39
getMode 9 = Sv48

-- Return the number of page table levels for a given mode.
pageTableLevels Sv32 = 2
pageTableLevels Sv39 = 3
pageTableLevels Sv48 = 4

-- Return the size of a PTE (in bytes) for a given mode.
pteSize Sv32 = 4
pteSize Sv39 = 8
pteSize Sv48 = 8

-- Return the size (in bits) of a PPN field (except the last) for a given mode.
ppnBits Sv32 = 10
ppnBits Sv39 = 9
ppnBits Sv48 = 9

-- Return the total length of all PPN fields for a given mode.
ppnLength Sv32 = 22
ppnLength Sv39 = 44
ppnLength Sv48 = 44

getVPN mode va i = bitSlice va (12 + i * ppnBits mode) (12 + (i + 1) * ppnBits mode)

loadXLEN :: (RiscvProgram p t) => t -> p MachineInt
loadXLEN addr = do
  xlen <- getXLEN
  if xlen == 32
    then fmap (fromIntegral:: Int32 -> MachineInt) (loadWord addr)
    else fmap (fromIntegral:: Int64 -> MachineInt) (loadDouble addr)

storeXLEN :: (RiscvProgram p t) => t -> MachineInt -> p ()
storeXLEN addr val = do
  xlen <- getXLEN
  if xlen == 32
    then storeWord addr ((fromIntegral:: MachineInt -> Int32) val)
    else storeDouble addr ((fromIntegral:: MachineInt -> Int64) val)

data AccessType = Instruction | Load | Store deriving (Eq, Show)

pageFault :: forall a p t. (RiscvProgram p t) => AccessType -> MachineInt -> p a
pageFault Instruction va = raiseExceptionWithInfo 0 12 va
pageFault Load va = raiseExceptionWithInfo 0 13 va
pageFault Store va = raiseExceptionWithInfo 0 15 va

-- Recursively traverse the page table to find the leaf entry for a given virtual address.
findLeafEntry :: forall p t. (RiscvProgram p t) => (VirtualMemoryMode, AccessType, MachineInt, MachineInt) -> Int -> p (Maybe (Int, MachineInt, MachineInt))
findLeafEntry (mode,accessType,va,addr) level = do
  let pteAddr = addr + (getVPN mode va level * pteSize mode)
  pte <- loadXLEN ((fromIntegral:: MachineInt -> t) pteAddr)
  let v = testBit pte 0
  let r = testBit pte 1
  let w = testBit pte 2
  let x = testBit pte 3
  -- TODO: PMA and PMP checks.
  if | not v || (not r && w) -> do
         pageFault accessType va
         return Nothing
     | r || x ->
         return (Just (level, pte, pteAddr))
     | level <= 0 -> do
         pageFault accessType va
         return Nothing
     -- This is duplicated for clash's sake.
     | level == 1 ->
         findLeafEntry (mode, accessType, va, (shift (bitSlice pte 10 (10 + ppnLength mode)) 12)) 0
     | level == 2 ->
         findLeafEntry (mode, accessType, va, (shift (bitSlice pte 10 (10 + ppnLength mode)) 12)) 1
     | level == 3 ->
         findLeafEntry (mode, accessType, va, (shift (bitSlice pte 10 (10 + ppnLength mode)) 12)) 2
     | level == 4 ->
         findLeafEntry (mode, accessType, va, (shift (bitSlice pte 10 (10 + ppnLength mode)) 12)) 3
     | otherwise -> return Nothing

translateHelper :: VirtualMemoryMode -> MachineInt -> MachineInt -> Int -> MachineInt
translateHelper mode va pte level = vaSlice .|. shift ptePPN vaSplit
  where vaSplit = 12 + level * (ppnBits mode)
        vaSlice = bitSlice va 0 vaSplit
        ptePPN = shiftR pte (10 + level * (ppnBits mode))

calculateAddress :: (RiscvProgram p t) => AccessType -> MachineInt -> p MachineInt
calculateAddress accessType va = do
  mode <- fmap getMode (getCSRField Field.MODE)
  privMode <- getPrivMode
  mprv <- getCSRField Field.MPRV
  mpp <- getCSRField Field.MPP
  let effectPriv = if mprv == 1 then decodePrivMode mpp else privMode
  if mode == None || (privMode == Machine && accessType == Instruction) || (effectPriv == Machine)
    then return va
    else -- First the translation may be in a cache, possibly stalled, cacheAccess use the typeclass defined "TLB"
      cacheAccess va $ 
       do
    ppn <- getCSRField Field.PPN
    maybePTE <- findLeafEntry (mode, accessType, va, (shift ppn 12)) (pageTableLevels mode - 1)
    case maybePTE of
      Nothing -> pageFault accessType va
      Just (level, pte, addr) -> do
        let r = testBit pte 1
        let w = testBit pte 2
        let x = testBit pte 3
        let u = testBit pte 4
        let a = testBit pte 6
        let d = testBit pte 7
        sum <- getCSRField Field.SUM
        mxr <- getCSRField Field.MXR
        -- Note that effectPriv must be either Supervisor or User at this point.
        let validUserAccess = (u && (effectPriv == User || sum == 1))
        let validSupervisorAccess = (not u && effectPriv == Supervisor)
        let validRead = (accessType == Load && (r || (x && mxr == 1)))
        let validExecute = (accessType == Instruction && x)
        let validWrite = (accessType == Store && w)
        if | not ((validUserAccess || validSupervisorAccess) && (validRead || validExecute || validWrite)) -> do
               -- Bad permissions.
               pageFault accessType va
           | level > 0 && bitSlice pte 10 (10 + level * ppnBits mode) /= 0 -> do
               -- Misaligned superpage.
               pageFault accessType va
           | not a || (accessType == Store && not d) -> do
               -- Set dirty/access bits in hardware:
               -- let newPTE = (pte .|. (bit 6) .|. (if accessType == Store then bit 7 else 0))
               -- storeXLEN (fromIntegral addr) newPTE
               -- return (translateHelper mode va newPTE level)
               -- Set dirty/access bits in software:
               pageFault accessType va
           | otherwise -> do
               -- Successful translation.
               return (translateHelper mode va pte level)

translate :: forall p t. (RiscvProgram p t) => AccessType -> Int -> t -> p t
translate accessType alignment va =  do
  pa <- calculateAddress accessType ((fromIntegral :: t -> MachineInt) va)
  if mod pa ((fromIntegral:: Int -> MachineInt) alignment) /= 0  -- Check alignment.
      -- TODO: Figure out if mtval should be set to pa or va here.
      then raiseExceptionWithInfo 0 misalignCode pa
      else return ((fromIntegral :: MachineInt -> t) pa)
  where misalignCode =
          case accessType of
            Instruction -> 0
            Load -> 4
            Store -> 6

