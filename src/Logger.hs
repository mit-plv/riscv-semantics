{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Logger where
import Control.Monad.Writer hiding (tell)
import Numeric
import Program
import Debug.Trace
import Data.Word

-- Workaround so that logs can print before the program has terminated. This
-- pretty well defeats the purpose of involving WriterT, so we should eventually
-- either sort out the laziness issues to make WriterT useful for this, or
-- just abandon it.
tell s = trace (take (length s - 1) s) (return ())

instance (RiscvProgram p t) => RiscvProgram (WriterT String p) t where
  getRegister r = do
    tell ("getRegister " ++ (show (fromIntegral r)) ++ "\n")
    lift (getRegister r)
  setRegister r v = do
    tell ("setRegister " ++ (show (fromIntegral r, fromIntegral v)) ++ "\n")
    lift (setRegister r v)
  loadByte a = do
    tell ("loadByte " ++ (show (fromIntegral a)) ++ "\n")
    lift (loadByte a)
  loadHalf a = do
    tell ("loadHalf " ++ (show (fromIntegral a)) ++ "\n")
    lift (loadHalf a)
  loadWord a = do
    tell ("loadWord " ++ (show (fromIntegral a)) ++ "\n")
    lift (loadWord a)
  loadDouble a = do
    tell ("loadDouble " ++ (show (fromIntegral a)) ++ "\n")
    lift (loadDouble a)
  storeByte a v = do
    tell ("storeByte " ++ (show (fromIntegral a, fromIntegral v)) ++ "\n")
    lift (storeByte a v)
  storeHalf a v = do
    tell ("storeHalf " ++ (show (fromIntegral a, fromIntegral v)) ++ "\n")
    lift (storeHalf a v)
  storeWord a v = do
    tell ("storeWord " ++ (show (fromIntegral a, fromIntegral v)) ++ "\n")
    lift (storeWord a v)
  storeDouble a v = do
    tell ("storeDouble " ++ (show (fromIntegral a, fromIntegral v)) ++ "\n")
    lift (storeDouble a v)
  getCSRField f = do
    tell ("getCSRField " ++ (show f) ++ "\n")
    lift (getCSRField f)
  setCSRField f v = do
    tell ("setCSRField " ++ (show (f, fromIntegral v)) ++ "\n")
    lift (setCSRField f v)
  getPC = do
    tell "getPC\n"
    lift getPC
  setPC v = do
    tell ("setPC 0x" ++ (showHex (fromIntegral v :: Word64) "\n"))
    lift (setPC v)
  getPrivMode = do
    tell "getPrivMode\n"
    lift getPrivMode
  setPrivMode v = do
    tell ("setPrivMode 0x" ++ (show v) ++  "\n")
    lift (setPrivMode v)
  commit = do
    tell "commit\n"
    lift commit
  endCycle = do
    tell "endCycle\n"
    lift endCycle
