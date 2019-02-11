
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
writeClint :: IORef Int32 -> Int32 -> Int32 -> IO (Bool)
writeClint mtimecmp addr val = do
  case (addr) of
    0x4000 -> undefined
    0x4004 -> undefined
    otherwise -> return False
  undefined

readClint :: IORef Int32 -> IORef Int32 -> Int32 -> IO (Maybe Int32)
readClint mtimecmp rtc addr = do
 case (addr) of
   0xbff8 -> fmap Just $ readIORef rtc
   0xbffc -> fmap (Just . (\x -> shiftR x 32)) $ readIORef rtc
   0x4000 -> fmap Just $ readIORef mtimecmp
   0x4004 -> fmap (Just . (\x -> shiftR x 32)) $ readIORef mtimecmp
   otherwise -> return Nothing

initClint :: IO (MVar [Word8], Fd)
initClint = do
  undefined
  -- (master,_) <- openPseudoTerminal
  -- file <- getSlaveTerminalName master
  -- attr <- getTerminalAttributes master
  -- setTerminalAttributes master (withOutputSpeed (withInputSpeed attr  B115200) B115200) Immediately
  -- list <- (newMVar [] :: IO (MVar [Word8]))
  -- putStrLn $ "Connect to console with : screen " ++ show file ++ " 115200"
  -- _ <- async $ poller master list
  -- putStrLn "Spawned the emulated pty"
  -- return (list, master)
