{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MultiWayIf, UndecidableInstances #-}
module  where
import Data.Bits
import Data.Int
import Data.Char
import BufferMMIO
import Control.Monad
import Control.Monad.Amb

import Program
import Utility

-- Simple State monad to simulate IO. The first string represents input, the
-- second represents output.
type BufferIO = State (String, String)

runBufferIO :: BufferIO a -> String -> (a, String)
runBufferIO m input = (result, output)
  where (result, (_, output)) = runState m (input, "")

type BufferState s = StateT s BufferIO

instance (RiscvProgram (BufferState s) t, MachineWidth t) => RiscvProgram ( s) t where
  getRegister r = liftState (getRegister r)
  setRegister r v = liftState (setRegister r v)
  loadByte a = liftState (loadByte a)
  loadHalf a = liftState (loadHalf a)
  loadWord addr = liftState (loadWord addr)
  loadDouble a = liftState (loadDouble a)
  storeByte a v = liftState (storeByte a v)
  storeHalf a v = liftState (storeHalf a v)
  storeWord addr val = liftState (storeWord addr val)
  storeDouble a v = liftState (storeDouble a v)
  getCSRField f = liftState (getCSRField f)
  setCSRField f v = liftState (setCSRField f v)
  getPC = liftState getPC
  setPC v = liftState (setPC v)
  getPrivMode = liftState getPrivMode
  setPrivMode v = liftState (setPrivMode v)
  commit = liftState commit
  endCycle = liftState endCycle
  inTLB a = liftState (inTLB a) -- noTLB
  addTLB a b c = liftState (addTLB a b c)
  
