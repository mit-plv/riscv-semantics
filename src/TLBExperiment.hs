{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MultiWayIf, UndecidableInstances #-}
module TLBExperiment where

import Data.Bits
import Data.Int
import Data.Char
import BufferMMIO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Amb

import Program
import Utility

-- Simple State monad to simulate IO. The first string represents input, the
-- second represents output.


instance (RiscvProgram (s) t, MachineWidth t) => RiscvProgram (AmbT r s) t where
  getRegister r = lift(getRegister r)
  setRegister r v = lift(setRegister r v)
  loadByte a = lift(loadByte a)
  loadHalf a = lift(loadHalf a)
  loadWord addr = lift(loadWord addr)
  loadDouble a = lift(loadDouble a)
  storeByte a v = lift(storeByte a v)
  storeHalf a v = lift(storeHalf a v)
  storeWord addr val = lift(storeWord addr val)
  storeDouble a v = lift(storeDouble a v)
  getCSRField f = lift(getCSRField f)
  setCSRField f v = lift(setCSRField f v)
  getPC = lift getPC
  setPC v = lift (setPC v)
  getPrivMode = lift getPrivMode
  setPrivMode v = lift (setPrivMode v)
  commit = lift commit
  endCycle = lift endCycle
  inTLB a = lift (inTLB a) -- noTLB
  addTLB a b c = lift (addTLB a b c)
  
