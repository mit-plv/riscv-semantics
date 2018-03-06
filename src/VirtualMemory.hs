{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

module VirtualMemory where
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

getVPN mode va i = bitSlice va 12 (12 + (i + 1) * ppnBits mode)

loadXLEN :: (RiscvProgram p t) => t -> p MachineInt
loadXLEN addr = do
  xlen <- getXLEN
  if xlen == 32
    then fmap (fromIntegral:: Int32 -> MachineInt) (loadWord addr)
    else fmap (fromIntegral:: Int64 -> MachineInt) (loadDouble addr)

data AccessType = Instruction | Load | Store deriving (Eq, Show)

pageFault :: (RiscvProgram p t) => AccessType -> p ()
pageFault Instruction = raiseException 0 12
pageFault Load = raiseException 0 13
pageFault Store = raiseException 0 15

-- Recursively traverse the page table to find the leaf entry for a given virtual address.
findLeafEntry :: forall p t u. (RiscvProgram p t) => (VirtualMemoryMode, AccessType, MachineInt, MachineInt) -> Int -> p (Maybe (Int, MachineInt))
findLeafEntry (mode,accessType,va,addr) level = do
  pte <- loadXLEN ((fromIntegral:: MachineInt -> t) (addr + (getVPN mode va level * pteSize mode)))
  let v = testBit pte 0
  let r = testBit pte 1
  let w = testBit pte 2
  let x = testBit pte 3
  -- TODO: PMA and PMP checks.
  if | not v || (r && w) -> do
         pageFault accessType
         return Nothing
     | r || x ->
         return (Just (level, pte))
     | level <= 0 -> do
         pageFault accessType
         return Nothing
     | level ==1 ->
         findLeafEntry (mode, accessType, va, (shift (shiftL pte 10) 12)) 0
     | level ==2 ->
         findLeafEntry (mode, accessType, va, (shift (shiftL pte 10) 12)) 1
     | level ==3 ->
         findLeafEntry (mode, accessType, va, (shift (shiftL pte 10) 12)) 2
     | level ==4 ->
         findLeafEntry (mode, accessType, va, (shift (shiftL pte 10) 12)) 3
     | otherwise -> return Nothing

translate :: VirtualMemoryMode -> MachineInt -> MachineInt -> Int -> MachineInt
translate mode va pte level = vaSlice .|. shift ptePPN vaSplit
  where vaSplit = 12 + level * (ppnBits mode)
        vaSlice = bitSlice va 0 vaSplit
        ptePPN = shiftL pte (10 + level * (ppnBits mode))

calculateAddress :: (RiscvProgram p t) => AccessType -> MachineInt -> p (Maybe MachineInt)
calculateAddress accessType va = do
  mode <- fmap getMode (getCSRField Field.MODE)
  if mode == None
    then return (Just va)
    else do
    ppn <- getCSRField Field.PPN
    maybePTE <- findLeafEntry (mode,accessType, va, (shift ppn 12) )(pageTableLevels mode - 1)
    case maybePTE of
      Nothing -> return Nothing
      Just (level, pte) -> do
        -- TODO: Raise page fault if the permissions are wrong. (Check r, w, x, u, mstatus.)
        let a = testBit pte 6
        let d = testBit pte 7
        if | level > 0 && bitSlice pte 10 (10 + level * ppnBits mode) /= 0 -> do
               pageFault accessType
               return Nothing
           | not a || (accessType == Store && not d) -> do
               pageFault accessType
               return Nothing
           | otherwise ->
               return (Just (translate mode va pte level))

withTranslation :: forall p t s. (RiscvProgram p t, Integral s) => AccessType -> Int -> s -> (s -> p ()) -> p ()
withTranslation accessType alignment va memFunc = do
  maybePA <- calculateAddress accessType ((fromIntegral:: s -> MachineInt) va)
  case maybePA of
    Just pa -> if mod pa ((fromIntegral:: Int -> MachineInt) alignment) /= 0  -- Check alignment.
               then raiseException 0 4
               else memFunc ((fromIntegral:: MachineInt -> s) pa)
    Nothing -> return ()
