{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MultiWayIf, UndecidableInstances #-}
module Platform.BufferMMIO where
import Data.Bits
import Data.Int
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as S

import Spec.Machine
import Utility.Utility

-- Simple State monad to simulate IO. The first string represents input, the
-- second represents output.
type BufferIO = State (String, String)

runBufferIO :: BufferIO a -> String -> (a, String)
runBufferIO m input = (result, output)
  where (result, (_, output)) = runState m (input, "")

type BufferState s = StateT s BufferIO

type LoadFunc s = BufferState s Int32
type StoreFunc s = Int32 -> BufferState s ()

instance (Show (LoadFunc s)) where
  show _ = "<io/loadfunc>"
instance (Show (StoreFunc s)) where
  show _ = "<io/storefunc>"

bufferGetChar :: BufferIO Int32
bufferGetChar = state $ \((input, output)) ->
                          if null input then (-1, (input, output))
                          else (fromIntegral $ ord $ head input, (tail input, output))
bufferPutChar :: Int32 -> BufferIO ()
bufferPutChar c = state $ \((input, output)) -> ((), (input, output ++ [chr $ fromIntegral c]))

rvGetChar :: LoadFunc s
rvGetChar = lift bufferGetChar
rvPutChar :: StoreFunc s
rvPutChar val = lift (bufferPutChar val)

rvZero :: LoadFunc s
rvZero = return 0
rvNull :: StoreFunc s
rvNull val = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
mmioTable :: S.Map MachineInt (LoadFunc s, StoreFunc s)
mmioTable = S.fromList [(0xfff0, (rvZero, rvPutChar)), (0xfff4, (rvGetChar, rvNull))]

instance (RiscvMachine (State s) t, MachineWidth t) => RiscvMachine (BufferState s) t where
  getRegister r = liftState (getRegister r)
  setRegister r v = liftState (setRegister r v)
  getFPRegister r = liftState (getFPRegister r)
  setFPRegister r v = liftState (setFPRegister r v)
  loadByte a = liftState (loadByte a)
  loadHalf a = liftState (loadHalf a)
  loadWord addr =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (getFunc, _) -> getFunc
      Nothing -> liftState (loadWord addr)
  loadDouble a = liftState (loadDouble a)
  storeByte a v = liftState (storeByte a v)
  storeHalf a v = liftState (storeHalf a v)
  storeWord addr val =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (_, setFunc) -> setFunc (fromIntegral val)
      Nothing -> liftState (storeWord addr val)
  storeDouble a v = liftState (storeDouble a v)
  makeReservation a = liftState (makeReservation a)
  checkReservation a = liftState (checkReservation a)
  clearReservation a = liftState (clearReservation a)
  getCSRField f = liftState (getCSRField f)
  setCSRField f v = liftState (setCSRField f v)
  getPC = liftState getPC
  setPC v = liftState (setPC v)
  getPrivMode = liftState getPrivMode
  setPrivMode v = liftState (setPrivMode v)
  commit = liftState commit
  endCycle = liftState endCycle
  inTLB a b = liftState (inTLB a b) -- noTLB
  addTLB a b c = liftState (addTLB a b c)
  flushTLB = liftState flushTLB
  getPlatform = liftState getPlatform
