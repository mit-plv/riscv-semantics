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
import Platform.Plic
import Platform.Clint
import Control.Concurrent.MVar

data VerifMinimal64 = VerifMinimal64 { registers :: IOUArray Register Int64 ,
                                       fpregisters :: IOUArray Register Int32,
                                       csrs :: CSRFile,
                                       privMode :: IORef PrivMode,
                                       pc :: IORef Int64,
                                       nextPC :: IORef Int64,
                                       mem :: IOUArray Int Word8,
                                       plic :: Plic,
                                       clint :: (IORef Int64, MVar Int64),
                                       console :: (MVar [Word8], Fd),
                                       reservation :: IORef (Maybe Int),
                                                     -- Verification Packet
                                       exception :: IORef Bool,
                                       interrupt :: IORef Bool,
                                       valid_dst :: IORef Bool,
                                       valid_addr :: IORef Bool,
                                       instruction :: IORef Int32,
                                       cause :: IORef Int32,
                                       d :: IORef Word64,
                                       dst :: IORef Int64,
                                       addrPacket :: IORef Word64,
                                       pcPacket :: IORef Int64,
                                       valid_timer :: IORef Bool,
                                       timer :: IORef Int64,
                                       mipPacket:: IORef Int64

                                     }


type IOState = StateT VerifMinimal64 IO

type LoadFunc = IOState Int32
type StoreFunc = Int32 -> IOState ()

rvGetChar :: IOState Int32
rvGetChar = do
  refs <- get
  mWord <- liftIO $ readPty (fst (console refs))
  lift $ putStrLn "Get Char happened"
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
getMTime = undefined

-- Ignore writes to mtime.
setMTime :: StoreFunc
setMTime _ = return ()


readPlicWrap addr = do
  refs <- get
  (val, interrupt) <- lift $ readPlic (plic refs) addr
  when (interrupt == Set) (do
                  lift $ putStrLn "Set external interrupt from read"
                  setCSRField Field.MEIP 1)
  when (interrupt == Reset) (do
                  lift $ putStrLn "Reset external initerrupt"
                  setCSRField Field.MEIP 0)
  return val
writePlicWrap addr val = do
  refs <- get
  interrupt <- lift $ writePlic (plic refs) addr val
  when (interrupt == Set) (do
                  lift $ putStrLn "Set external interrupt from write"
                  setCSRField Field.MEIP 1)
  when (interrupt == Reset) (do
                  lift $ putStrLn "Reset external interrupt from write"
                  setCSRField Field.MEIP 0)
  return ()

readClintWrap addr = do
  refs <- get
  let (mtimecmp,rtc) = clint refs
  mint <- lift $ readClint mtimecmp rtc addr
  lift $ writeIORef (valid_timer refs) True
  when (addr == 0xbff8) . lift . writeIORef (timer refs) . fromIntegral . fromJust $ mint
--  lift . putStrLn $ "readClint " ++ show mint ++ " at addr " ++ show addr
  case mint of
    Just a -> return a
    Nothing -> return 0 --Impossible
writeClintWrap addr val = do
  refs <- get
  let (mtimecmp,rtc) = clint refs
  lift . putStrLn $ "writeClint " ++ show val ++ " at addr " ++ show addr
  lift $ writeClint mtimecmp addr val
  setCSRField Field.MTIP 0

-- Addresses for mtime/mtimecmp chosen for Spike compatibility, and getchar putchar.
memMapTable :: S.Map MachineInt (LoadFunc, StoreFunc)
memMapTable = S.fromList
              [
              -- Pty
               (0xfff0, (rvZero, rvPutChar)),
               (0xfff4, (rvGetChar, rvNull)),
               -- Plic
               (0x4000000, (readPlicWrap 0x200000,  writePlicWrap 0x200000)),
               (0x4000004, (readPlicWrap 0x200004, writePlicWrap 0x200004)),
               -- Clint
               (0x2000000, (fmap fromIntegral $ getCSRField Field.MSIP, setCSRField Field.SSIP)),
               (0x200bff8, (readClintWrap 0xbff8, writeClintWrap 0xbff8)),
               (0x200bffc, (readClintWrap 0xbffc, writeClintWrap 0xbffc)),
               (0x2004000, (readClintWrap 0x4000, writeClintWrap 0x4000)),
               (0x2004004, (readClintWrap 0x4004, writeClintWrap 0x4004))
 
              ]

mtimecmp_addr = 0x4000 :: Int64

instance RiscvMachine IOState Int64 where
  getRegister reg = do
      if reg == 0
      then return 0
      else do
        refs <- get
        lift $! readArray (registers refs) reg
  setRegister reg val = do
      refs <- get
      if reg == 0
      then do
      --  lift $ writeIORef (valid_dst refs) True
      --  lift $ writeIORef (dst refs) reg
      --  lift $ writeIORef (d refs) $ fromIntegral val
        return ()
      else do
        lift $ writeIORef (valid_dst refs) True
        lift $ writeIORef (dst refs) reg
        lift $ writeIORef (d refs) $ fromIntegral val
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
  loadByte addr = 
       case S.lookup (fromIntegral addr) memMapTable of
       Just _ -> error "loadByte on MMIO unsupported" 
       Nothing -> do
         refs <- get
         fmap fromIntegral . lift $ readArray (mem refs) (fromIntegral addr)
  loadHalf addr = 
       case S.lookup (fromIntegral addr) memMapTable of
       Just _ -> error "loadHalf on MMIO unsupported" 
       Nothing -> do
          refs <- get
          b0 <- lift . readArray (mem refs) $ fromIntegral addr
          b1 <- lift . readArray (mem refs) $ fromIntegral (addr + 1)
          return (combineBytes [b0,b1])
  loadWord :: forall s. (Integral s) => s -> IOState Int32
  loadWord ad = do
    val <- (case S.lookup ((fromIntegral:: s -> MachineInt) ad) memMapTable of
      Just (getFunc, _) -> getFunc
      Nothing -> do
       refs <- get
       b0 <- lift . readArray (mem refs) $! fromIntegral ad
       b1 <- lift . readArray (mem refs) $! fromIntegral (ad + 1)
       b2 <- lift . readArray (mem refs) $! fromIntegral (ad + 2)
       b3 <- lift . readArray (mem refs) $! fromIntegral (ad + 3)
       return (combineBytes [b0,b1,b2,b3]))
    return val
  loadDouble addr = do
       res_bot <- loadWord addr
       res_top <- loadWord (addr+4)
       let bytes_bot = splitWord res_bot
       let bytes_top = splitWord res_top
       return (combineBytes $ bytes_bot ++ bytes_top)
  storeByte addr val =
       case S.lookup (fromIntegral addr) memMapTable of
       Just _ -> error "storeByte on MMIO unsupported"
       Nothing -> do
          refs <- get
          lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral val) -- Convert from Int8 to Word8
  storeHalf addr val =  
       case S.lookup (fromIntegral addr) memMapTable of
       Just _ -> error "storeHald on MMIO unsupported"
       Nothing -> do
         let bytes = splitHalf val
         refs <- get
         forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
  storeWord :: forall s. (Integral s, Bits s) => s -> Int32 -> IOState ()
  storeWord addr val = do
    refs <- get
    lift $ writeIORef (valid_addr refs) True
    lift $ writeIORef (addrPacket refs) $ fromIntegral addr
    lift $ writeIORef (d refs) . fromIntegral $ (fromIntegral val :: Word32)
   -- when (addr >= 0x2000000 && addr < 0x20c0000) .lift $ putStrLn ("write to the clint:  " ++ show ( fromIntegral addr))
    case S.lookup ((fromIntegral:: s -> MachineInt) addr) memMapTable of
      Just (_, setFunc) -> setFunc val
      Nothing -> do 
       let bytes = splitWord val
      -- refs <- get
       forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
  storeDouble addr val =
    case (S.lookup (fromIntegral addr) memMapTable,S.lookup (fromIntegral (addr+4)) memMapTable) of
    (Just (_, setFunc1 ),Just (_, setFunc2 ))  -> do
           setFunc1 $ fromIntegral (val .&. 0xFFFFFFFF)  
           setFunc2 $ fromIntegral (shiftR val 32)
    (Nothing, Nothing) -> do
       let bytes = splitDouble val
       refs <- get
       forM_ (zip bytes [addr + i| i<- [0..]])  $ (\(x,addr)-> lift $ writeArray (mem refs) (fromIntegral addr) (fromIntegral x))
    _ -> error "storeDouble half within MMIO, half without that's SOOOO wrong"
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

