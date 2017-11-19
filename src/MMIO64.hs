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
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO.Error
import qualified Data.Map as S

import qualified Minimal64 as Min

newtype IOMState s a = IOMState { runState :: s -> IO (a, s) }

instance Functor (IOMState s) where
  fmap f a = IOMState $ \state -> do
    (b, s) <- runState a state
    return (f b, s)

instance Applicative (IOMState s) where
  pure x = IOMState $ \state -> return (x, state)
  (<*>) f a = IOMState $ \state -> do
    (f', s') <- runState f state
    (a', s'') <- runState a s'
    return (f' a', s'')

instance Monad (IOMState s) where
  (>>=) a f = IOMState $ \state -> do
    (b, s) <- runState a state
    runState (f b) s

liftMState :: Min.MState s a -> IOMState s a
liftMState f = IOMState $ \state -> return (Min.runState f state)

ord32 :: Char -> Int32
ord32 = fromIntegral . ord

chr32 :: Int32 -> Char
chr32 = chr . fromIntegral

type LoadFunc = IOMState Min.Minimal64 Int32
type StoreFunc = Int32 -> IOMState Min.Minimal64 ()

instance (Show LoadFunc) where
  show x = "<io/loadfunc>"
instance (Show StoreFunc) where
  show x = "<io/storefunc>"

cGetChar :: IO Int32
cGetChar = catchIOError (fmap ord32 getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: LoadFunc
rvGetChar = IOMState $ \comp -> liftIO cGetChar >>= (\c -> return (c, comp))
rvPutChar :: StoreFunc
rvPutChar val = IOMState $ \comp -> liftIO (putChar $ chr32 val) >> return ((), comp)

getMTime :: LoadFunc
getMTime = fmap fromIntegral (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime val = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility.
mmioTable :: S.Map MachineInt (LoadFunc, StoreFunc)
mmioTable = S.fromList [(0xfff4, (rvGetChar, rvPutChar)),
                        (0x200bff8, (getMTime, setMTime))]
mtimecmp_addr = 0x2004000

instance RiscvProgram (IOMState Min.Minimal64) Int64 Word64 where
  getXLEN = liftMState getXLEN
  getRegister r = liftMState (getRegister r)
  setRegister r v = liftMState (setRegister r v)
  loadByte a = liftMState (loadByte a)
  loadHalf a = liftMState (loadHalf a)
  loadWord addr =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (getFunc, _) -> getFunc
      Nothing -> liftMState (loadWord addr)
  loadDouble a = liftMState (loadDouble a)
  storeByte a v = liftMState (storeByte a v)
  storeHalf a v = liftMState (storeHalf a v)
  storeWord addr val =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (_, setFunc) -> setFunc (fromIntegral val)
      Nothing -> liftMState (storeWord addr val)
  storeDouble a v = liftMState (storeDouble a v)
  getCSRField f = liftMState (getCSRField f)
  setCSRField f v = liftMState (setCSRField f v)
  getPC = liftMState getPC
  setPC v = liftMState (setPC v)
  step = liftMState step
