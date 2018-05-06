{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, MultiWayIf, UndecidableInstances #-}
module TLBExperiment where

import Data.Bits
import Data.Int
import Data.Char
import Data.Maybe
import Data.List.Index
import qualified Data.Map as Map
import CSRFile
import qualified CSRField as Field
import BufferMMIO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Amb
import Control.Monad.State
import VirtualMemory
import Program
import Utility

-- Simple State monad to simulate IO. The first string represents input, the
-- second represents output.
type Tlb = (State [Map.Map MachineInt (MachineInt,Int)])
type TlbState s = StateT s Tlb 

getTlbs :: Tlb ([Map.Map MachineInt (MachineInt,Int)])
getTlbs = state $ \l -> (l,l)
putTlbs :: [Map.Map MachineInt (MachineInt,Int)] -> Tlb ()
putTlbs new = state $ \l -> ((),new)



instance (RiscvProgram (State s) t, MachineWidth t) => RiscvProgram (TlbState   s  ) t where
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
  inTLB a = do
           tlbLevels <- lift getTlbs
           mode <- fmap getMode (getCSRField Field.MODE)
           let founds = imap (\idx tlbLevel->Map.lookup (getVPN mode a (idx+1)) tlbLevel) tlbLevels 
           return . listToMaybe . catMaybes $ fmap (\found -> case found of 
                          Just (pte,level) -> Just $ translateHelper mode a pte level
                          Nothing -> Nothing) founds 
                           
  addTLB addr pte level = do
      mode <- fmap getMode (getCSRField Field.MODE)
      tlbLevels <- lift getTlbs
      lift . putTlbs . imap (\idx tlbLevel -> 
                   if (idx == level -1) 
                     then Map.insert (getVPN mode addr level) (pte, level) tlbLevel 
                     else tlbLevel
                  ) $ tlbLevels
  
