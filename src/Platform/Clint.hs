
module Platform.Clint
  (
  readClint,
  writeClint,
  initClint) where
import qualified Data.ByteString as B
import System.Posix.Types (Fd)
import Data.ByteString.Char8 (unpack)
import Control.Concurrent.Async
import Data.IORef
import Data.Int
import Data.Bits
import Control.Concurrent.MVar
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word
import Control.Exception

-- Returns a bool that tells if we should set MIP_MTIP
writeClint :: IORef Int64 -> Int32 -> Int32 -> IO (Bool)
writeClint mtimecmp addr val = do
  case (addr) of
    0x4000 -> do
      oldTime <- readIORef mtimecmp
      writeIORef mtimecmp $ (oldTime .&. (complement 0xffffffff)) .|. (fromIntegral ((fromIntegral  val) :: Word32) :: Int64)
      return True
    0x4004 -> do
      oldTime <- readIORef mtimecmp
      writeIORef mtimecmp $ (oldTime .&. 0xffffffff) .|. (shiftL (fromIntegral ((fromIntegral  val) :: Word32) :: Int64) 32)
      return True
    _ -> return False

readClint :: IORef Int64 -> MVar Int64 -> Int32 -> IO (Maybe Int32)
readClint mtimecmp rtc addr = do
 case (addr) of
   0xbff8 -> fmap (Just . fromIntegral) $ readMVar rtc
   0xbffc -> fmap (Just . fromIntegral . (\x -> shiftR x 32)) $ readMVar rtc
   0x4000 -> fmap (Just . fromIntegral) $ readIORef mtimecmp
   0x4004 -> fmap (Just . fromIntegral . (\x -> shiftR x 32)) $ readIORef mtimecmp
   _ -> return Nothing


-- Returns mtimecmp and rtc.
initClint :: IO (IORef Int64, MVar Int64)
initClint = do
  mtimecmp <- newIORef 0 :: IO(IORef Int64)
  rtc <- newMVar 0 :: IO(MVar Int64)
  return (mtimecmp,rtc)
  -- (master,_) <- openPseudoTerminal
  -- file <- getSlaveTerminalName master
  -- attr <- getTerminalAttributes master
  -- setTerminalAttributes master (withOutputSpeed (withInputSpeed attr  B115200) B115200) Immediately
  -- list <- (newMVar [] :: IO (MVar [Word8]))
  -- putStrLn $ "Connect to console with : screen " ++ show file ++ " 115200"
  -- _ <- async $ poller master list
  -- putStrLn "Spawned the emulated pty"
  -- return (list, master)
