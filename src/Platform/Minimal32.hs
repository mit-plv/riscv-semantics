{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs #-}
module Platform.Minimal32 where
import Spec.Machine
import Spec.Decode
import Utility.Utility
import Spec.CSRFile
import qualified Spec.CSRField as Field
import qualified Spec.Memory as M
import Utility.MapMemory
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Map as S
import Control.Monad.State

data Minimal32 = Minimal32 { registers :: [Int32], csrs :: CSRFile, pc :: Int32,
                             nextPC :: Int32, privMode :: PrivMode, mem :: MapMemory Int }
               deriving (Show)

type MState = State Minimal32

type LoadFunc = MState Int32
type StoreFunc = Int32 -> MState ()

instance (Show LoadFunc) where
  show _ = "<loadfunc>"
instance (Show StoreFunc) where
  show _ = "<storefunc>"

getMTime :: LoadFunc
getMTime = fmap (fromIntegral:: MachineInt -> Int32) (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime _ = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
memMapTable :: S.Map MachineInt (LoadFunc, StoreFunc)
memMapTable = S.fromList [(0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000

wrapLoad :: forall a' r r' m. (Integral a', Integral r, Integral r') => (MapMemory Int -> Int -> r) -> (a' -> MState r')
wrapLoad loadFunc addr = state $ \comp -> ((fromIntegral:: r -> r') $ loadFunc (mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)), comp)
wrapStore :: forall a' v v' m. (Integral a', Integral v, Integral v') => (MapMemory Int -> Int -> v -> MapMemory Int) -> (a' -> v' -> MState ())
wrapStore storeFunc addr val = state $ \comp -> ((), comp { mem = storeFunc (mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)) ((fromIntegral:: v' -> v) val) })

instance RiscvMachine MState Int32 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! ((fromIntegral:: Register -> Int) reg-1), comp)
  setRegister :: forall s. (Integral s) => Register -> s -> MState ()
  setRegister reg val = state $ \comp -> ((), if reg == 0 then comp else comp { registers = setIndex ((fromIntegral:: Register -> Int) reg-1) ((fromIntegral:: s -> Int32) val) (registers comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC :: forall s. (Integral s) => s -> MState ()
  setPC val = state $ \comp -> ((), comp { nextPC = (fromIntegral:: s -> Int32) val })
  getPrivMode = state $ \comp -> (privMode comp, comp)
  setPrivMode val = state $ \comp -> ((), comp { privMode = val })
  commit = do
    -- Post interrupt if mtime >= mtimecmp
    mtime <- getMTime
    mtimecmp <- loadWord Execute mtimecmp_addr
    setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
    -- Check for interrupts before updating PC.
    mie <- getCSRField Field.MIE
    meie <- getCSRField Field.MEIE
    meip <- getCSRField Field.MEIP
    mtie <- getCSRField Field.MTIE
    mtip <- getCSRField Field.MTIP
    nPC <- state $ \comp -> (nextPC comp, comp)
    fPC <- (if (mie > 0 && ((meie > 0 && meip > 0) || (mtie > 0 && mtip > 0))) then do
              -- Disable interrupts
              setCSRField Field.MIE 0
              if (meie > 0 && meip > 0) then do
                -- Remove pending external interrupt
                setCSRField Field.MEIP 0
                setCSRField Field.MCauseCode 11 -- Machine external interrupt.
              else if (mtie > 0 && mtip > 0) then
                setCSRField Field.MCauseCode 7 -- Machine timer interrupt.
              else return ()
              -- Save the PC of the next (unexecuted) instruction.
              setCSRField Field.MEPC nPC
              trapPC <- getCSRField Field.MTVecBase
              return ((fromIntegral:: MachineInt -> Int32) trapPC * 4)
            else return nPC)
    state $ \comp -> ((), comp { pc = fPC })
  -- Wrap Memory instance:
  loadByte s = wrapLoad M.loadByte
  loadHalf s = wrapLoad M.loadHalf
  loadWord :: forall s. (Integral s) => SourceType -> s -> MState Int32
  loadWord s addr =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> wrapLoad M.loadWord addr
  storeByte s = wrapStore M.storeByte
  storeHalf s = wrapStore M.storeHalf
  storeWord :: forall s. (Integral s, Bits s) => SourceType -> s -> Int32 -> MState ()
  storeWord s addr val =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> wrapStore M.storeWord addr val
  makeReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word32 -> Int) ((fromIntegral :: Int32 -> Word32) addr)) })
  checkReservation addr = state $ \comp -> (M.checkReservation (mem comp) ((fromIntegral :: Word32 -> Int) ((fromIntegral :: Int32 -> Word32) addr)), comp)
  clearReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word32 -> Int) ((fromIntegral :: Int32 -> Word32) addr)) })
  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  unsafeSetCSRField :: forall s. (Integral s) => Field.CSRField -> s -> MState ()
  unsafeSetCSRField field val = state $ \comp -> ((), comp { csrs = setField field ((fromIntegral:: s -> MachineInt) val) (csrs comp) })
  -- Unimplemented:
  loadDouble s _ = return 0
  storeDouble s _ _ = return ()
  inTLB a b = return Nothing -- noTLB
  addTLB a b c= return ()
  flushTLB = return ()
