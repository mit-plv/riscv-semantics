{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes #-}
module Platform.Minimal64 where
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
import Data.Maybe
import qualified Data.Map.Strict as S
import Control.Monad.State

data Minimal64 = Minimal64 { registers :: S.Map Register Int64,
                             -- TODO: May need to replace this with a strict map to avoid a memory leak.

                             fpregisters :: [Int32],
                             csrs :: CSRFile,
                             pc :: Int64,
                             nextPC :: Int64,
                             privMode :: PrivMode,
                             mem :: MapMemory Int
                           } deriving (Show)

type MState = State Minimal64


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
mtimecmp_addr = 0x2004000 :: Int64

wrapLoad :: forall a' r r' m. (Integral a', Integral r, Integral r') => (MapMemory Int -> Int -> r) -> (a' -> MState r')
wrapLoad loadFunc addr = state $ \comp -> ((fromIntegral:: r -> r') $ loadFunc (mem comp) ((fromIntegral:: Word64 -> Int) ((fromIntegral:: a' -> Word64) addr)), comp)
wrapStore :: forall a' v v' m. (Integral a', Integral v, Integral v') => (MapMemory Int -> Int -> v -> MapMemory Int) -> (a' -> v' -> MState ())
wrapStore storeFunc addr val = state $ \comp -> ((), comp { mem = storeFunc (mem comp) ((fromIntegral:: Word64 -> Int) ((fromIntegral:: a' -> Word64) addr)) ((fromIntegral:: v' -> v) val) })

instance RiscvMachine MState Int64 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else fromMaybe 0 (S.lookup reg (registers comp)), comp)
  setRegister :: forall s. (Integral s) => Register -> s -> MState ()
  setRegister reg val = state $ \comp -> ((), if reg == 0 then comp else comp { registers = S.insert reg (fromIntegral val) (registers comp) })
  getFPRegister reg = state $ \comp -> ((fpregisters comp) !! ((fromIntegral:: Register -> Int) reg), comp)
  setFPRegister :: forall s. (Integral s) => FPRegister -> s -> MState ()
  setFPRegister reg val = state $ \comp -> ((), comp { fpregisters = setIndex ((fromIntegral:: Register -> Int) reg) ((fromIntegral:: s -> Int32) val) (fpregisters comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC :: forall s. (Integral s) => s -> MState ()
  setPC val = state $ \comp -> ((), comp { nextPC = (fromIntegral:: s -> Int64) val })
  getPrivMode = state $ \comp -> (privMode comp, comp)
  setPrivMode val = state $ \comp -> ((), comp { privMode = val })
  commit = state $ \comp -> ((), comp { pc = nextPC comp })
  -- Wrap Memory instance:
  loadByte s = wrapLoad M.loadByte
  loadHalf s = wrapLoad M.loadHalf
  loadWord :: forall s. (Integral s) => SourceType -> s -> MState Int32
  loadWord s addr =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> wrapLoad M.loadWord addr
  loadDouble s  = wrapLoad M.loadDouble
  storeByte s = wrapStore M.storeByte
  storeHalf s = wrapStore M.storeHalf
  storeWord :: forall s. (Integral s, Bits s) => SourceType -> s -> Int32 -> MState ()
  storeWord s addr val =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> wrapStore M.storeWord addr val
  storeDouble s = wrapStore M.storeDouble
  makeReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)) })
  checkReservation addr = state $ \comp -> (M.checkReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)), comp)
  clearReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)) })

  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  unsafeSetCSRField :: forall s. (Integral s) => Field.CSRField -> s -> MState ()
  unsafeSetCSRField field val = state $ \comp -> ((), comp { csrs = setField field ((fromIntegral:: s -> MachineInt) val) (csrs comp) })
  inTLB a b = return Nothing -- noTLB
  addTLB a b c= return ()
  flushTLB = return ()

  getPlatform = return (Platform { dirtyHardware = return False, writePlatformCSRField = \field value -> return value })
