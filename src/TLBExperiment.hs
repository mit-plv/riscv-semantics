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
type Tlb = ([Map.Map MachineInt (MachineInt,Int)])
type TlbState s = StateT Tlb s 

--getTlbs :: Tlb ([Map.Map MachineInt (MachineInt,Int)])
--getTlbs = state $ \l -> (l,l)
--putTlbs :: [Map.Map MachineInt (MachineInt,Int)] -> Tlb ()
--putTlbs new = state $ \l -> ((),new)
--runTlb :: Tlb a -> [Map.Map MachineInt (MachineInt,Int)] -> a
--runTlb m input = fst $ runState m input
--



instance (RiscvProgram (s) t, MachineWidth t) => RiscvProgram (TlbState   s  ) t where
  getRegister r = lift (getRegister r)
  setRegister r v = lift(setRegister r v)
  loadByte a = lift (loadByte a)
  loadHalf a = lift (loadHalf a)
  loadWord addr = lift (loadWord addr)
  loadDouble a = lift (loadDouble a)
  storeByte a v = lift (storeByte a v)
  storeHalf a v = lift (storeHalf a v)
  storeWord addr val = lift (storeWord addr val)
  storeDouble a v = lift (storeDouble a v)
  getCSRField f = lift (getCSRField f)
  setCSRField f v = lift (setCSRField f v)
  getPC = lift getPC
  setPC v = lift (setPC v)
  getPrivMode = lift getPrivMode
  setPrivMode v = lift (setPrivMode v)
  commit = lift commit
  endCycle = lift endCycle
  inTLB a = do
           tlbLevels <- get
           mode <- fmap getMode (getCSRField Field.MODE)
           let founds = imap (\idx tlbLevel->Map.lookup (getVPN mode a (idx+1)) tlbLevel) tlbLevels 
           return . listToMaybe . catMaybes $ fmap (\found -> case found of 
                          Just (pte,level) -> Just $ translateHelper mode a pte level
                          Nothing -> Nothing) founds 
                           
  addTLB addr pte level = do
      mode <- fmap getMode (getCSRField Field.MODE)
      tlbLevels <- get
      put . imap (\idx tlbLevel -> 
                   if (idx == level -1) 
                     then Map.insert (getVPN mode addr level) (pte, level) tlbLevel 
                     else tlbLevel
                  ) $ tlbLevels
  
