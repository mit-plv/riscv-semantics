
-- TODO catch the EAGAIN to try again after a little bit of sleep.
-- TODO in getC is this malloc/free reasonable?
-- TODO there should be better primitive to read one byte of a Fd that I may have missed

module Platform.Pty
  (
  readPty,
  writePty,
  initPty) where
import System.Posix.Terminal
import System.Posix.IO
import qualified Data.ByteString as B
import System.Posix.Types (Fd)
import Data.ByteString.Char8 (unpack)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word
import Control.Exception


getC master = do
  bytePtr <- malloc :: IO (Ptr Word8)
  bytecount <- fdReadBuf master bytePtr 1
  if (bytecount /= 0)
    then do
    putStrLn "Keyboard event"
    tl <- peek bytePtr
    free bytePtr --Is this safe?
    return $! tl
    else do
    free bytePtr
    error "EOF sent by the pty"

poller master list =
  iterateChar
  where
    iterateChar = do
      byte <- getC master
      l <- takeMVar list
      putMVar list (l  ++ [ byte ])
      iterateChar

writePty :: Fd -> Word8 -> IO ()
writePty master byte = do
  count <- fdWrite master . unpack $ B.singleton byte
  if (count /= 1)
  then
    error "write to pty failed"
  else
    return ()

readPty :: MVar [Word8] -> IO (Maybe Word8)
readPty list = do
  l <- takeMVar list
  if l == []
    then do
    putMVar list l
    return Nothing
    else do
    putMVar list (tail l)
    return . Just $ head l

initPty :: IO (MVar [Word8], Fd)
initPty = do
  (master,_) <- openPseudoTerminal
  file <- getSlaveTerminalName master
  attr <- getTerminalAttributes master
  setTerminalAttributes master (withOutputSpeed (withInputSpeed attr  B115200) B115200) Immediately
  list <- (newMVar [] :: IO (MVar [Word8]))
  putStrLn $ "Connect to console with : screen " ++ show file ++ " 115200"
  _ <- async $ poller master list
  putStrLn "Spawned the emulated pty"
  return (list, master)
