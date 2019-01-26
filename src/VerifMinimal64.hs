{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes #-}
module VerifMinimal64 where
import Program
import Decode
import Utility
import CSRFile
import qualified CSRField as Field
import qualified Memory as M
import MapMemory
import Data.Bits
import Data.Int
import Data.Word
import Data.Maybe
import qualified Data.Map.Strict as S
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Debug.Trace as T

data VerifMinimal64 = VerifMinimal64 { registers :: S.Map Register Int64,
                             fpregisters :: [Int32],
                             valid_dst :: Bool,
                             valid_addr :: Bool,
                             d :: Word64,
                             instruction :: Int32,
                             cause :: Int64,
                             exception :: Bool,
                             interrupt :: Bool,
                             addr :: Word64,
                             dst :: Int64,
                             csrs :: CSRFile,
                             pc :: Int64,
                             pcPacket :: Int64,
                             nextPC :: Int64,
                             privMode :: PrivMode,
                             mem :: MapMemory Int,
                             fromHost :: Maybe Int64,
                             valid_timer ::  Bool,
                             timer :: Int32 
                           } deriving (Show)

type MState = State VerifMinimal64

type LoadFunc = MState Int32
type StoreFunc = Int32 -> MState ()

instance (Show LoadFunc) where
  show _ = "<loadfunc>"
instance (Show StoreFunc) where
  show _ = "<storefunc>"

getMTime :: LoadFunc
getMTime = do
 timer <- fmap (fromIntegral:: MachineInt -> Int32) (getCSRField Field.MCycle)
 s <- get
 put (s{valid_timer = True, timer = timer})
 return timer


getMTimeInt :: LoadFunc
getMTimeInt = do
 timer <- fmap (fromIntegral:: MachineInt -> Int32) (getCSRField Field.MCycle)
 return timer
 
-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime _ = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
memMapTable :: S.Map MachineInt (LoadFunc, StoreFunc)
memMapTable = S.fromList [(0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000 :: Int64

wrapLoad :: forall a' r r' m. (Integral a', Integral r, Integral r') => (MapMemory Int -> Int -> r) -> (a' -> MState r')
wrapLoad loadFunc addr = (if addr == 0x87e6bf7c || addr == 0x87e6bf78 || addr == 0x87e6bf7e || addr == 0x87e6bf7f then (\x-> trace ( "LOAD FROM THE ADDRESS " ++ (show $ fromIntegral addr) ) x)
 else id) . state $ \comp -> ((fromIntegral:: r -> r') $ loadFunc (mem comp) ((fromIntegral:: Word64 -> Int) ((fromIntegral:: a' -> Word64) addr)), comp)
wrapStore :: forall a' v v' m. (Integral a', Integral v, Integral v') => (MapMemory Int -> Int -> v -> MapMemory Int) -> (a' -> v' -> MState ())
wrapStore storeFunc addr val = 
 (if addr == 0x87e6bf7c || addr == 0x87e6bf78 || addr == 0x87e6bf7e || addr == 0x87e6bf7f then (\x-> trace ("WRITE TO THE ADDRESS " ++ (show $ fromIntegral val)) x)
 else id) . state $ \comp -> ((), comp { mem = storeFunc (mem comp) ((fromIntegral:: Word64 -> Int) ((fromIntegral:: a' -> Word64) addr)) ((fromIntegral:: v' -> v) val), addr= fromIntegral addr, valid_addr=True, d = (fromIntegral :: v -> Word64) . (fromIntegral:: v' -> v) $ val})

instance RiscvProgram MState Int64 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (fromMaybe 0 $ S.lookup reg (registers comp)) , comp)
  setRegister :: forall s. (Integral s) => Register -> s -> MState ()
  setRegister reg val = state $ \comp -> ((), if reg == 0 then comp else comp {valid_dst = True, dst= fromIntegral reg, d=fromIntegral val, registers = S.insert reg ((fromIntegral:: s -> Int64) val) (registers comp) })
  getFPRegister reg = state $ \comp -> ((fpregisters comp) !! ((fromIntegral:: Register -> Int) reg), comp)
  setFPRegister :: forall s. (Integral s) => FPRegister -> s -> MState ()
  setFPRegister reg val = state $ \comp -> ((), comp { fpregisters = setIndex ((fromIntegral:: Register -> Int) reg) ((fromIntegral:: s -> Int32) val) (fpregisters comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC :: forall s. (Integral s) => s -> MState ()
  setPC val = state $ \comp -> ((), comp { nextPC = (fromIntegral:: s -> Int64) val })
  getPrivMode = state $ \comp -> (privMode comp, comp)
  setPrivMode val = state $ \comp -> ((), comp { privMode = val })
  commit = do
    -- Post interrupt if mtime >= mtimecmp
      state $ \comp -> ((), comp{pc=nextPC comp})
  -- Wrap Memory instance:
  loadByte = wrapLoad M.loadByte
  loadHalf = wrapLoad M.loadHalf
  loadWord :: forall s. (Integral s) => s -> MState Int32
  loadWord addr =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> wrapLoad M.loadWord addr
  loadDouble = wrapLoad M.loadDouble
  storeByte = wrapStore M.storeByte
  storeHalf = wrapStore M.storeHalf
  storeWord :: forall s. (Integral s, Bits s) => s -> Int32 -> MState ()
  storeWord addr val =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> wrapStore M.storeWord addr val
  storeDouble = wrapStore M.storeDouble
  makeReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)) })
  checkReservation addr = state $ \comp -> (M.checkReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)), comp)
  clearReservation addr = state $ \comp -> ((), comp { mem = M.makeReservation (mem comp) ((fromIntegral :: Word64 -> Int) ((fromIntegral :: Int64 -> Word64) addr)) })

  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  setCSRField :: forall s. (Integral s) => Field.CSRField -> s -> MState ()
  setCSRField field val = state $ \comp -> ((), comp { csrs = setField field ((fromIntegral:: s -> MachineInt) val) (csrs comp) })
  inTLB a b = return Nothing -- noTLB
  addTLB a b c= return ()
  flushTLB = return ()