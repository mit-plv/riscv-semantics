{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module MMIO64 where
import Data.Int
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import System.IO.Error
import qualified Data.Map as S

import Program
import Utility
import qualified Minimal64 as Min

type IOMState = StateT Min.Minimal64 IO

type LoadFunc = IOMState Int32
type StoreFunc = Int32 -> IOMState ()

instance (Show LoadFunc) where
  show _ = "<io/loadfunc>"
instance (Show StoreFunc) where
  show _ = "<io/storefunc>"

cGetChar :: IO Int32
cGetChar = catchIOError (fmap (fromIntegral . ord) getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: LoadFunc
rvGetChar = liftIO cGetChar
rvPutChar :: StoreFunc
rvPutChar val = liftIO (putChar $ chr $ fromIntegral val)

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
mmioTable :: S.Map MachineInt (LoadFunc, StoreFunc)
mmioTable = S.fromList [(0xfff4, (rvGetChar, rvPutChar))]

liftState :: (Monad m) => State a b -> StateT a m b
liftState = mapStateT (return . runIdentity)

instance RiscvProgram IOMState Int64 Word64 where
  getXLEN = liftState getXLEN
  getRegister r = liftState (getRegister r)
  setRegister r v = liftState (setRegister r v)
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
  getCSRField f = liftState (getCSRField f)
  setCSRField f v = liftState (setCSRField f v)
  getPC = liftState getPC
  setPC v = liftState (setPC v)
  step = liftState step
