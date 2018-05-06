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
import Debug.Trace

-- Simple State monad to simulate IO. The first string represents input, the
-- second represents output.
type Tlb = ([Map.Map MachineInt (MachineInt,Int)])
type TlbState s = StateT Tlb s 


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
  inTLB accessType a = do
           mode <- fmap getMode (getCSRField Field.MODE)
           tlbLevels <- get
           let founds = imap (\idx tlbLevel->Map.lookup (getVPN mode a (idx+1)) tlbLevel) tlbLevels 
           return . listToMaybe . catMaybes $ imap (\idx found -> case found of 
                          Just (pte,level) ->  
                               let abit = testBit pte 6
                                   d = testBit pte 7
                               in 
                               if (not abit || (accessType == Store && not d)) 
                                 then Nothing 
                                 else Just $ translateHelper mode a pte level
                          Nothing ->  Nothing) founds 
                           
  addTLB addr pte level = do
      mode <- fmap getMode (getCSRField Field.MODE)
      tlbLevels <- get
      put . imap (\idx tlbLevel -> 
                   if (idx == level -1) 
                     then Map.insert (getVPN mode addr level) (pte, level) tlbLevel 
                     else tlbLevel
                  ) $ tlbLevels
  flushTLB = put [Map.empty,Map.empty, Map.empty, Map.empty] 
