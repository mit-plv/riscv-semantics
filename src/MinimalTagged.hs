{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs #-}
module Minimal32 where
import Program
import Decode
import Utility
import CSRFile
import qualified CSRField as Field
import qualified Memory as M
import MapMemory()
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Map as S
import Control.Monad.State

data Minimal32 = Minimal32 { registers :: [Int32], csrs :: CSRFile, pc :: Int32,
                             nextPC :: Int32, privMode :: PrivMode, mem :: S.Map Int (Tag,Word32) }
               deriving (Show)

type MState = State Minimal32

type LoadFunc = MState (Tag,Int32)
type StoreFunc = (Tag,Int32) -> MState ()

instance (Show LoadFunc) where
  show _ = "<loadfunc>"
instance (Show StoreFunc) where
  show _ = "<storefunc>"

getMTime :: LoadFunc
getMTime = fmap fromIntegral (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime _ = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
memMapTable :: S.Map MachineInt (LoadFunc, StoreFunc)
memMapTable = S.fromList [(0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000

data Tag = Bare | Overflowed deriving (Show,Eq)
instance Integral (Tag,Int32) where
-- write toInteger and fromInteger

instance RiscvProgram MState (Tag,Int32) where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! ((fromIntegral:: Register -> Int) reg-1), comp)
  setRegister :: forall s. (Integral s) => Register -> s -> MState ()
  setRegister reg val = state $ \comp -> ((), if reg == 0 then comp else comp { registers = setIndex ((fromIntegral:: Register -> Int) reg-1) (fromIntegral val) (registers comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC :: forall s. (Integral s) => s -> MState ()
  setPC val = state $ \comp -> ((), comp { nextPC = fromIntegral val })
  getPrivMode = state $ \comp -> (privMode comp, comp)
  setPrivMode val = state $ \comp -> ((), comp { privMode = val })
  commit = do
    -- Post interrupt if mtime >= mtimecmp
    mtime <- getMTime
    mtimecmp <- loadWord mtimecmp_addr
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
              return ((fromIntegral) trapPC * 4)
            else return nPC)
    state $ \comp -> ((), comp { pc = fPC })
  -- Wrap Memory instance:
  loadByte = undefined
  loadHalf = undefined
  loadWord :: forall s. (Integral s) => s -> MState (Tag,Int32)
  loadWord addr =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> state $ \comp -> ((mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)), comp)
  storeByte = undefined
  storeHalf = undefined
  storeWord :: forall s. (Integral s, Bits s) => s -> (Tag,Int32) -> MState ()
  storeWord addr val =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> state $ \comp -> ((), comp { mem = storeFunc (mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)) ((fromIntegral:: v' -> v) val) })
  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  setCSRField :: forall s. (Integral s) => Field.CSRField -> s -> MState ()
  setCSRField field val = state $ \comp -> ((), comp { csrs = setField field ((fromIntegral:: s -> MachineInt) val) (csrs comp) })
  -- Unimplemented:
  loadDouble _ = return 0
  storeDouble _ _ = return ()
