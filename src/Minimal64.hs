{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Minimal64 where
import Program
import Utility
import CSRFile
import qualified CSRField as Field
import qualified Memory as M
import MapMemory
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Map as S
import Control.Applicative
import Control.Monad

newtype MState s a = MState { runState :: s -> (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> let (b, s) = runState a state in (f b, s)

instance Applicative (MState s) where
  pure x = MState $ \state -> (x, state)
  (<*>) f a = MState $ \state ->
    (let (f', s') = runState f state
         (a', s'') = runState a s' in
       (f' a', s''))

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> let (b, s) = runState a state in runState (f b) s

data Minimal64 = Minimal64 { registers :: [Int64], csrs :: CSRFile, pc :: Int64,
                             nextPC :: Int64, mem :: S.Map Int Word8 }
               deriving (Show)

type LoadFunc = MState Minimal64 Int32
type StoreFunc = Int32 -> MState Minimal64 ()

instance (Show LoadFunc) where
  show x = "<loadfunc>"
instance (Show StoreFunc) where
  show x = "<storefunc>"

getMTime :: LoadFunc
getMTime = fmap fromIntegral (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime val = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
mmioTable :: S.Map MachineInt (LoadFunc, StoreFunc)
mmioTable = S.fromList [(0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000

wrapLoad loadFunc addr = MState $ \comp -> (fromIntegral $ loadFunc (mem comp) addr, comp)
wrapStore storeFunc addr val = MState $ \comp -> ((), comp { mem = storeFunc (mem comp) addr (fromIntegral val) })

instance RiscvProgram (MState Minimal64) Int64 Word64 where
  getXLEN = return 64
  getRegister reg = MState $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  getPC = MState $ \comp -> (pc comp, comp)
  setPC val = MState $ \comp -> ((), comp { nextPC = fromIntegral val })
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
    nPC <- MState $ \comp -> (nextPC comp, comp)
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
    MState $ \comp -> ((), comp { pc = fPC })
  -- Wrap Memory instance:
  loadByte = wrapLoad M.loadByte
  loadHalf = wrapLoad M.loadHalf
  loadWord = wrapLoad M.loadWord
  loadDouble = wrapLoad M.loadDouble
  storeByte = wrapStore M.storeByte
  storeHalf = wrapStore M.storeHalf
  storeWord = wrapStore M.storeWord
  storeDouble = wrapStore M.storeDouble
  -- CSRs:
  getCSRField field = MState $ \comp -> (getField field (csrs comp), comp)
  setCSRField field val = MState $ \comp -> ((), comp { csrs = setField field (fromIntegral val) (csrs comp) })
