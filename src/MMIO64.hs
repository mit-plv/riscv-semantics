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

newtype MState s a = MState { runState :: s -> IO (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> fmap (\(b,s) -> (f b, s)) (runState a state)

instance Applicative (MState s) where
  pure x = MState $ \state -> return (x, state)
  (<*>) f a = MState $ \state -> do
    (g, s1) <- runState f state
    (b, s2) <- runState a s1
    return (g b, s2)

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> runState a state >>= (\(b, s) -> runState (f b) s)

data MMIO64 = MMIO64 { registers :: [Int64], csrs :: CSRFile, pc :: Int64,
                       nextPC :: Int64, mem :: S.Map Int Word8 }
              deriving (Show)

-- open, close, read, write

type LoadFunc = MState MMIO64 Int32
type StoreFunc = Int32 -> MState MMIO64 ()

instance (Show LoadFunc) where
  show x = "<loadfunc>"
instance (Show StoreFunc) where
  show x = "<storefunc>"

ord32 :: Char -> Int32
ord32 = fromIntegral . ord

chr32 :: Int32 -> Char
chr32 = chr . fromIntegral

cGetChar :: IO Int32
cGetChar = catchIOError (fmap ord32 getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: LoadFunc
rvGetChar = MState $ \comp -> liftIO cGetChar >>= (\c -> return (c, comp))
rvPutChar :: StoreFunc
rvPutChar val = MState $ \comp -> liftIO (putChar $ chr32 val) >> return ((), comp)

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

wrapLoad loadFunc addr = MState $ \comp -> return (fromIntegral $ loadFunc (mem comp) addr, comp)
wrapStore storeFunc addr val = MState $ \comp -> return ((), comp { mem = storeFunc (mem comp) addr (fromIntegral val) })

instance RiscvProgram (MState MMIO64) Int64 Word64 where
  getXLEN = return 64
  getRegister reg = MState $ \comp -> return (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> return ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  getPC = MState $ \comp -> return (pc comp, comp)
  setPC val = MState $ \comp -> return ((), comp { nextPC = fromIntegral val })
  step = do
    -- Post interrupt if mtime >= mtimecmp
    mtime <- getMTime
    mtimecmp <- loadWord mtimecmp_addr
    setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
    -- Check for interrupts before updating PC.
    mie <- getCSRField Field.MIE
    meie <- getCSRField Field.MEIE
    meip <- getCSRField Field.MEIP
    mtie <- getCSRField Field.MTIE
    mtip <- getCSRField Field.MTIP
    nPC <- MState $ \comp -> (return (nextPC comp, comp))
    fPC <- (if (mie > 0 && ((meie > 0 && meip > 0) || (mtie > 0 && mtip > 0))) then do
              -- Disable interrupts
              setCSRField Field.MIE 0
              if (meie > 0 && meip > 0) then do
                -- Remove pending external interrupt
                setCSRField Field.MEIP 0
                setCSRField Field.MCauseCode 11 -- Machine external interrupt.
              else if (mtie > 0 && mtip > 0) then
                setCSRField Field.MCauseCode 7 -- Machine timer interrupt.
              else return ()
              -- Save the PC of the next (unexecuted) instruction.
              setCSRField Field.MEPC nPC
              trapPC <- getCSRField Field.MTVecBase
              return (trapPC * 4)
            else return nPC)
    MState $ \comp -> (return ((), comp { pc = fPC }))

  -- Memory functions:
  loadByte = wrapLoad M.loadByte
  loadHalf = wrapLoad M.loadHalf
  loadWord addr =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (getFunc, _) -> getFunc
      Nothing -> (MState (\comp -> return (fromIntegral $ M.loadWord (mem comp) addr, comp)))
  loadDouble = wrapLoad M.loadDouble
  storeByte = wrapStore M.storeByte
  storeHalf = wrapStore M.storeHalf
  storeWord addr val =
    case S.lookup (fromIntegral addr) mmioTable of
      Just (_, setFunc) -> setFunc (fromIntegral val)
      Nothing -> (MState (\comp -> return ((), comp { mem = M.storeWord (mem comp) addr (fromIntegral val) })))
  storeDouble = wrapStore M.storeDouble
  -- CSRs:
  getCSRField field = MState $ \comp -> return (getField field (csrs comp), comp)
  setCSRField field val = MState $ \comp -> return ((), comp { csrs = setField field (fromIntegral val) (csrs comp) })
