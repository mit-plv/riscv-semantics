{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Logger where
import Control.Monad.Writer hiding (tell)
import Numeric
import Program
import Utility
import Debug.Trace
import Data.Word

-- Workaround so that logs can print before the program has terminated. This
-- pretty well defeats the purpose of involving WriterT, so we should eventually
-- either sort out the laziness issues to make WriterT useful for this, or
-- just abandon it.
tell s = do
  pc <- lift getPC
  let c = showHex (fromIntegral pc :: Word64) ": "
  trace (c ++ s) (return ())

liftTell s op = do
  tell s
  lift op

liftTellShow s op = do
  m <- lift op
  tell (s ++ " = " ++ (show m))
  return m

liftTellInt s op = do
  m <- lift op
  tell (s ++ " = " ++ (show (fromIntegral m)))
  return m

instance (RiscvProgram p t) => RiscvProgram (WriterT String p) t where
  getRegister r = do
    liftTellInt ("getRegister " ++ (show (fromIntegral r))) (getRegister r)
  setRegister r v = do
    liftTell ("setRegister " ++ (show (fromIntegral r, fromIntegral v))) (setRegister r v)
  loadByte a = do
    liftTellInt ("loadByte " ++ (show (fromIntegral a))) (loadByte a)
  loadHalf a = do
    liftTellInt ("loadHalf " ++ (show (fromIntegral a))) (loadHalf a)
  loadWord a = do
    liftTellInt ("loadWord " ++ (show (fromIntegral a))) (loadWord a)
  loadDouble a = do
    liftTellInt ("loadDouble " ++ (show (fromIntegral a))) (loadDouble a)
  storeByte a v = do
    liftTell ("storeByte " ++ (show (fromIntegral a, fromIntegral v))) (storeByte a v)
  storeHalf a v = do
    liftTell ("storeHalf " ++ (show (fromIntegral a, fromIntegral v))) (storeHalf a v)
  storeWord a v = do
    liftTell ("storeWord " ++ (show (fromIntegral a, fromIntegral v))) (storeWord a v)
  storeDouble a v = do
    liftTell ("storeDouble " ++ (show (fromIntegral a, fromIntegral v))) (storeDouble a v)
  getCSRField f = do
    liftTellInt ("getCSRField " ++ (show f)) (getCSRField f)
  setCSRField f v = do
    liftTell ("setCSRField " ++ (show (f, fromIntegral v))) (setCSRField f v)
  getPC = do
    liftTellInt "getPC" getPC
  setPC v = do
    liftTell ("setPC 0x" ++ (showHex (fromIntegral v :: Word64) "")) (setPC v)
  getPrivMode = do
    liftTellShow "getPrivMode" getPrivMode
  setPrivMode v = do
    liftTell ("setPrivMode 0x" ++ (show v)) (setPrivMode v)
  commit = do
    liftTell "commit" commit
  endCycle = do
    liftTell "endCycle" endCycle
