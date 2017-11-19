{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module MMIO64 where
import qualified Memory as M
import MapMemory
import Program
import Utility
import CSRFile
import qualified CSRField as Field
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.IO.Error
import qualified Data.Map as S

import qualified Minimal64 as Min

type IOMState = StateT Min.Minimal64 IO

ord32 :: Char -> Int32
ord32 = fromIntegral . ord

chr32 :: Int32 -> Char
chr32 = chr . fromIntegral

type LoadFunc = IOMState Int32
type StoreFunc = Int32 -> IOMState ()

instance (Show LoadFunc) where
  show x = "<io/loadfunc>"
instance (Show StoreFunc) where
  show x = "<io/storefunc>"

cGetChar :: IO Int32
cGetChar = catchIOError (fmap ord32 getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: LoadFunc
rvGetChar = liftIO cGetChar
rvPutChar :: StoreFunc
rvPutChar val = liftIO (putChar $ chr32 val)

-- I have no idea what I'm doing
getSuccessReg :: LoadFunc
getSuccessReg = MState $ \comp -> return (0, comp)
setSuccessReg :: StoreFunc
setSuccessReg val = MState $ \comp -> liftIO (putStr "Return Code: " >> print val) >> return ((), comp)

getMTime :: LoadFunc
getMTime = fmap fromIntegral (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime val = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
mmioTable :: S.Map MachineInt (LoadFunc, StoreFunc)
mmioTable = S.fromList [(0xfff4, (rvGetChar, rvPutChar)),
                        (0x1000000, (getSuccessReg, setSuccessReg)),
                        (0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000

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
