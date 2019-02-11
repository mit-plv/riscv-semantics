{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes, FlexibleContexts #-}
module Platform.CleanTest where
import Spec.Machine
import Spec.Decode
import Utility.Utility
import Spec.CSRFileIO
import qualified Spec.CSRField as Field
import Data.Bits
import Data.Int
import Data.Word
import Data.Char
import Data.Maybe
import Data.IORef
import Data.Array.IO
import System.Posix.Types
import System.IO.Error
import qualified Data.Map.Strict as S
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Debug.Trace as T
import Platform.Pty
import Control.Concurrent.MVar

data VerifMinimal64 = VerifMinimal64 { registers :: IOUArray Register Int64 ,
                                       fpregisters :: IOUArray Register Int32,
                                       csrs :: CSRFile,
                                       privMode :: IORef PrivMode,
                                       pc :: IORef Int64,
                                       nextPC :: IORef Int64,
                                       mem :: IOUArray Int Word8,
                                       console :: (MVar [Word8], Fd),
                                       reservation :: IORef (Maybe Int)
                                     }


type IOState = StateT VerifMinimal64 IO

type LoadFunc = IOState Int32
type StoreFunc = Int32 -> IOState ()

-- instance (Show LoadFunc) where
--    show _ = "<loadfunc>"
-- instance (Show StoreFunc) where
--    show _ = "<storefunc>"

cGetChar :: IO Int32
cGetChar = catchIOError (fmap ((fromIntegral:: Int -> Int32). ord) getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: IOState Int32
rvGetChar = do
  refs <- get
  mWord <- liftIO $ readPty (fst (console refs))
  case mWord of
    Nothing -> return $ -1
    Just a -> return $ fromIntegral a

rvPutChar :: Int32 -> IOState ()
rvPutChar val = do
  refs <- get
  liftIO $ writePty (snd (console refs)) (fromIntegral val)

rvZero = return 0
rvNull val = return ()

getMTime :: LoadFunc
getMTime = fmap ((\x-> quot x 1000000).(fromIntegral:: MachineInt -> Int32)) (getCSRField Field.MCycle)

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime _ = return ()

-- Addresses for mtime/mtimecmp chosen for Spike compatibility, and getchar putchar.
memMapTable :: S.Map MachineInt (LoadFunc, StoreFunc)
memMapTable = S.fromList [(0x200bff8, (getMTime, setMTime)),(0xfff0, (rvZero, rvPutChar)), (0xfff4, (rvGetChar, rvNull))]
mtimecmp_addr = 0x2004000 :: Int64


instance RiscvMachine IOState Int64 where
  getRegister reg = do
       if reg == 0
         then return 0
         else do
           refs <- get
           lift $! readArray (registers refs) reg
  setRegister reg val = do
       if reg == 0
         then return ()
         else do
           refs <- get
           lift $! writeArray (registers refs) reg val
  getFPRegister reg = do
       refs <- get
       lift $! readArray (fpregisters refs) reg
  setFPRegister reg val = do
       refs <- get
       lift $! writeArray (fpregisters refs) reg val
  getPC = do
       refs <- get
       lift $! readIORef (pc refs)
  setPC npc = do
       refs <- get
       lift $! writeIORef (nextPC refs) npc
  getPrivMode = do
       refs <- get
       lift $! readIORef (privMode refs)
  setPrivMode val = do
       refs <- get
       lift $! writeIORef (privMode refs) val
  commit = do
       refs <- get
       npc <- lift $ readIORef (nextPC refs)
       lift $! writeIORef (pc refs) npc
--  -- Wrap Memory instance:
  loadByte addr = do
       refs <- get
       fmap fromIntegral . lift $ readArray (mem refs) (fromIntegral addr)
  loadHalf addr = do
       refs <- get
       b0 <- lift . readArray (mem refs) $ fromIntegral addr
       b1 <- lift . readArray (mem refs) $ fromIntegral (addr + 1)
       return (combineBytes [b0,b1])
  loadWord :: forall s. (Integral s) => s -> IOState Int32
  loadWord addr =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> do
       refs <- get
       b0 <- lift . readArray (mem refs) $! fromIntegral addr
       b1 <- lift . readArray (mem refs) $! fromIntegral (addr + 1)
       b2 <- lift . readArray (mem refs) $! fromIntegral (addr + 2)
       b3 <- lift . readArray (mem refs) $! fromIntegral (addr + 3)
       return (combineBytes [b0,b1,b2,b3])
  loadDouble addr = do
       refs <- get
       b0 <- lift . readArray (mem refs) $! fromIntegral addr
       b1 <- lift . readArray (mem refs) $! fromIntegral (addr + 1)
       b2 <- lift . readArray (mem refs) $! fromIntegral (addr + 2)
       b3 <- lift . readArray (mem refs) $! fromIntegral (addr + 3)
       b4 <- lift . readArray (mem refs) $! fromIntegral (addr + 4)
       b5 <- lift . readArray (mem refs) $! fromIntegral (addr + 5)
       b6 <- lift . readArray (mem refs) $! fromIntegral (addr + 6)
       b7 <- lift . readArray (mem refs) $! fromIntegral (addr + 7)
       return (combineBytes [b0,b1,b2,b3,b4,b5,b6,b7])
  storeByte addr val = do
       refs <- get
       lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral val) -- Convert from Int8 to Word8
  storeHalf addr val = do
       let bytes = splitHalf val
       refs <- get
       forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
  storeWord :: forall s. (Integral s, Bits s) => s -> Int32 -> IOState ()
  storeWord addr val =
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> do 
       let bytes = splitWord val
       refs <- get
       forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
  storeDouble addr val = do
       let bytes = splitDouble val
       refs <- get
       forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
  makeReservation addr = do
       refs <- get
       lift $ writeIORef (reservation refs) (Just $ fromIntegral addr)
  checkReservation addr = do
       refs <- get
       res <- lift $ readIORef (reservation refs)
       return (Just (fromIntegral addr) == res)
  clearReservation addr = do
       refs <- get
       lift $ writeIORef (reservation refs) Nothing
--  -- CSRs:
  getCSRField field = do
     refs <- get
     lift $! readArray (csrs refs) field
  unsafeSetCSRField :: forall s. (Integral s) => Field.CSRField -> s -> IOState ()
  unsafeSetCSRField field val = do -- CSRS are not refs in there, because I am lazy.
     refs <- get
     lift $! writeArray (csrs refs) field ((fromIntegral:: s -> MachineInt) val)
  inTLB a b = return Nothing -- noTLB
  addTLB a b c= return ()
  flushTLB = return ()
  getPlatform = return (Platform { dirtyHardware = return False, writePlatformCSRField = \field value -> return value })

