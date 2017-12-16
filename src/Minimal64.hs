{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Minimal64 where
import Program
import Utility
import CSRFile
import qualified CSRField as Field
import qualified Memory as M
import MapMemory()
import Data.Int
import Data.Word
import qualified Data.Map as S
import Control.Monad.State

data Minimal64 = Minimal64 { registers :: [Int64], csrs :: CSRFile, pc :: Int64,
                             nextPC :: Int64, mem :: S.Map Int Word8 }
               deriving (Show)

type MState = State Minimal64

type LoadFunc = MState Int32
type StoreFunc = Int32 -> MState ()

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

wrapLoad :: (Integral a, Integral a', Integral r, Integral r') => (S.Map Int Word8 -> a -> r) -> (a' -> MState r')
wrapLoad loadFunc addr = state $ \comp -> (fromIntegral $ loadFunc (mem comp) (fromIntegral (fromIntegral addr :: Word64)), comp)
wrapStore :: (Integral a, Integral a', Integral v, Integral v') => (S.Map Int Word8 -> a -> v -> S.Map Int Word8) -> (a' -> v' -> MState ())
wrapStore storeFunc addr val = state $ \comp -> ((), comp { mem = storeFunc (mem comp) (fromIntegral (fromIntegral addr :: Word64)) (fromIntegral val) })

instance RiscvProgram MState Int64 Word64 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp -> ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC val = state $ \comp -> ((), comp { nextPC = fromIntegral val })
  step = do
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
              return (trapPC * 4)
            else return nPC)
    state $ \comp -> ((), comp { pc = fPC })
  -- Wrap Memory instance:
  loadByte = wrapLoad M.loadByte
  loadHalf = wrapLoad M.loadHalf
  loadWord addr =
    case S.lookup (fromIntegral addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> wrapLoad M.loadWord addr
  loadDouble = wrapLoad M.loadDouble
  storeByte = wrapStore M.storeByte
  storeHalf = wrapStore M.storeHalf
  storeWord addr val =
    case S.lookup (fromIntegral addr) memMapTable of
      Just (_, setFunc) -> setFunc (fromIntegral val)
      Nothing -> wrapStore M.storeWord addr val
  storeDouble = wrapStore M.storeDouble
  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  setCSRField field val = state $ \comp -> ((), comp { csrs = setField field (fromIntegral val) (csrs comp) })
