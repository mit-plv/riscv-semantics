{-# OPTIONS -Wall #-}
module Platform.RunFast where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.IORef
import Data.Array.IO
import Data.List
import Data.Word
import Data.Bits
import Utility.Utility
import Spec.Machine
import Platform.CleanTest
import Utility.Elf
import qualified Spec.CSRField as Field
import Spec.CSRFileIO
import Spec.CSRSpec
import Spec.CSR
import Spec.Decode
import Spec.Spec
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Numeric (readHex, showHex)
import Platform.Pty
import Platform.Plic
import Platform.Clint
import Control.Concurrent.MVar
import System.CPUTime

processLine :: String -> (Int, [(Int, Word8)]) -> (Int, [(Int, Word8)])
processLine ('@':xs) (_, l) = ((fst $ head $ readHex xs) * 4, l)
processLine s (p, l) = (p + 4, l ++ (zip [p..] $ splitWord (fst $ head $ readHex s :: Word32)))

readHexFile :: FilePath -> IO [(Int, Word8)]
readHexFile f = do
  h <- openFile f ReadMode
  helper h (0, [])
  where helper h l = do
          s <- hGetLine h
          done <- hIsEOF h
          if (null s)
            then return $ snd l
            else if done
                 then return $ snd $ processLine s l
                 else helper h (processLine s l)

runProgram :: Maybe Int64 -> VerifMinimal64 -> IO (Int64, VerifMinimal64)
runProgram maybeToHostAddress c = do
  handle <- openFile "/tmp/hs2tandem" ReadWriteMode
  runStateT (stepHelper RV64IMAF maybeToHostAddress
            -- Check external interrupt
             (lift $(do
                 refs <- get
                 (do -- Restore state to nothing
                     exception <- lift $ writeIORef (exception refs) False
                     interrupt  <- lift $ writeIORef (interrupt refs) False
                     valid_dst <- lift $ writeIORef (valid_dst refs) False
                     valid_addr <- lift $ writeIORef (valid_addr refs) False
                     valid_timer <- lift $ writeIORef (valid_timer refs) False
                     instruction <- lift $ writeIORef (instruction refs) 0
                     cause <- lift $ writeIORef (cause refs) 0
                     d <- lift $ writeIORef (d refs) 0
                     dst <- lift $ writeIORef (dst refs) 0
                     addrPacket <- lift $ writeIORef (addrPacket refs) 0
                     pcPacket <- lift $ writeIORef (pcPacket refs) 0
                     timer <- lift $ writeIORef (timer refs) 0
                     lift $ writeIORef (mipPacket refs) 0
                     return ())
                 let (_, rtc) = clint refs
                 time <- getCSRField Field.MCycle
--                 mie <- getCSRField Field.MIE
                 --pc <- getPC
                 --lift . putStrLn $ showHex (fromIntegral pc :: Word64) ""
                 _ <- lift $ takeMVar rtc
                 lift . putMVar rtc $ (fromIntegral $ time `div` 1024) -- Time goes 10000 times slower for the RV
                 plicAlive <- lift $ takeMVar (toDo (plic refs))
                 lift $ putMVar (toDo (plic refs)) DoNothing
                 return plicAlive
                 ))
             -- Get timer and mtimer from the CLINT need more work refactorization
             (lift $(do
                 refs <- get
                 pc <- getPC
                 lift $  writeIORef (pcPacket refs) pc
                 let (mtimecmp, rtc) = clint c
                 vmtimecmp <- lift $ readIORef mtimecmp
                 vrtc <- lift $ readMVar rtc
                 return $! (vmtimecmp, vrtc)))
             (\inst -> do
                 mepc <- getCSR MEPC
--                 when (inst == 0x30200073) . lift . lift . putStrLn $ "mepc on mret is" ++ show mepc
                 refs <- lift $ get
                 lift . lift  $ writeIORef (instruction refs) inst
                 pc <- getPC
                 lift . lift $  writeIORef (pcPacket refs) pc
                 return (inst /= 0x6f)) -- Predecode
             (do
                 refs <- get
                 mtval <- getCSR MTVec
                 stval <- getCSR STVec
                 mipP <- getCSR MIP
                 lift . writeIORef (mipPacket refs) . fromIntegral $ (fromIntegral mipP :: Word64)
                 (do
                     npc <- lift $ readIORef (nextPC refs) 
--                     lift . writeIORef (pcPacket refs) $ npc
                     let trappedM = npc == (mtval .&. (complement 3)) -- && ! 3
                     let trappedS = npc == (stval .&. (complement 3)) -- && ! 3
                     if trappedM
                       then do
                       isInterruptM <- getCSRField Field.MCauseInterrupt
                       let isInterrupt = isInterruptM == 1
                       codeM <- getCSRField Field.MCauseCode
                       lift $ writeIORef (exception refs) (not isInterrupt)
                       lift $ writeIORef (interrupt refs) (isInterrupt)
                       lift $ writeIORef (cause refs) (fromIntegral codeM)
                       else
                       if trappedS
                       then do
                         isInterruptS <- getCSRField Field.SCauseInterrupt
                         let isInterrupt = isInterruptS == 1
                         codeS <- getCSRField Field.SCauseCode
                         lift $ writeIORef (exception refs) (not isInterrupt)
                         lift $ writeIORef (interrupt refs) (isInterrupt)
                         lift $ writeIORef (cause refs) (fromIntegral codeS)
                       else do
                         lift $ writeIORef (exception refs) False
                         lift $ writeIORef (interrupt refs) False
                         lift $ writeIORef (cause refs) 0)
--                 pcPacket <-  lift $ readIORef (pcPacket refs)
--                 instruction <- lift $ readIORef (instruction refs)
--                 exception <- lift $ readIORef (exception refs)
--                 interrupt <- lift $ readIORef (interrupt refs)
--                 cause <- lift $ readIORef (cause refs)
--                 addr <- lift $ readIORef (addrPacket refs)
--                 valid_addr <- lift $ readIORef (valid_addr refs)
--                 d <- lift $ readIORef (d refs)
--                 valid_dst <- lift $ readIORef (valid_dst refs)
--                 dst <- lift $ readIORef (dst refs)
--                 valid_timer <- lift $ readIORef (valid_timer refs)
--                 timer <- lift $ readIORef (timer refs)
--                 mipPacket <- lift $ readIORef (mipPacket refs)
--                 lift $ hPutStrLn handle "s"
--                 lift . hPutStrLn handle . show  $ pcPacket
--                 lift . hPutStrLn handle . show  $ instruction
--                 lift . hPutStrLn handle . show . fromEnum $ exception
--                 lift . hPutStrLn handle . show . fromEnum $ interrupt
--                 lift . hPutStrLn handle . show  $ cause
--                 lift . hPutStrLn handle . show  $ addr
--                 lift . hPutStrLn handle . show . fromEnum $ valid_addr
--             -- data is what we load or what we want to store
--                 lift . hPutStrLn handle . show  $ d
--                 lift . hPutStrLn handle . show . fromEnum $ valid_dst
--             -- dst is the idx of destination register
--                 lift . hPutStrLn handle . show  $ dst
--                 lift . hPutStrLn handle . show  . fromEnum $ valid_timer
--                 lift . hPutStrLn handle . show  $ timer
--                 lift . hPutStrLn handle . show  $ mipPacket
--                 lift $ hPutStrLn handle "e"
                 ) -- Precommit
            )
             c
     where





readProgram :: String -> IO (Maybe Int64, [(Int, Word8)])
readProgram f = do
  if ".hex" `isSuffixOf` f
    then do
    mem <- readHexFile f
    return (Nothing, mem)
    else do
    mem <- readElf f
    maybeToHostAddress <- readElfSymbol "tohost" f
    return (fmap (fromIntegral:: Word64 -> Int64) maybeToHostAddress, mem)

runFile :: String -> IO Int64
runFile f = do
  deviceTree <- B.readFile "device_tree.bin"
  (maybeToHostAddress, program) <- readProgram f
  -- Create the references and the arrays that are going to be passed around
  registers <- newArray (0,31) 0
  pc <- newIORef 0x80000000
  fpregisters <- newArray (0,31) 0
  npc <- newIORef 0
  privMode <- newIORef Machine
  mem <- newArray (0,0x00000000ffffffff) 0 -- Create a big 2GB chunk of memory
  reservation <- newIORef Nothing
  csrs <- newArray (Field.MXL,Field.FRM) 0 --GUESS TO GET ALL THE CSRFIELDS
  plic <- initPlic
  putStrLn "init PTY"
  console <- initPty plic
  clint <- initClint
  writeArray csrs Field.MXL 2
  writeArray csrs Field.Extensions $! encodeExtensions "IAMSU"
  putStrLn "All the state is created"

  -- Create Refs for verification packet
  exception <- newIORef False
  interrupt  <- newIORef False
  valid_dst <- newIORef False
  valid_addr <- newIORef False
  valid_timer <- newIORef False
  instruction <- newIORef 0
  cause <- newIORef 0
  d <- newIORef 0
  dst <- newIORef 0
  addrPacket <- newIORef 0
  pcPacket <- newIORef 0
  timer <- newIORef 0
  mipPacket <- newIORef 0
  -- Create device tree and program
  let addressCommaByteS = (zip [0..] (B.unpack deviceTree)) ++ program
  forM_ addressCommaByteS  $ (\(addr,byte)-> writeArray mem (fromIntegral addr) (fromIntegral byte))
  putStrLn "The program is copied"
  let c = VerifMinimal64 { registers = registers,
                           fpregisters = fpregisters,
                           csrs = csrs,
                           pc = pc,
                           nextPC = npc,
                           privMode = privMode,
                           mem = mem,
                           plic = plic,
                           clint = clint,
                           console = console,
                           reservation = reservation,
                           -- Verification packet:
                           exception = exception,
                           interrupt = interrupt,
                           valid_dst = valid_dst,
                           valid_addr = valid_addr,
                           valid_timer = valid_timer,
                           timer = timer,
                           instruction = instruction,
                           cause = cause,
                           d = d,
                           dst = dst,
                           addrPacket = addrPacket,
                           pcPacket = pcPacket,
                           mipPacket = mipPacket
                           } in
    fmap fst $ runProgram maybeToHostAddress c

runFiles :: [String] -> IO Int64
runFiles (file:files) = do
    myreturn <- runFile file
    putStr (file ++ ": " ++ (show myreturn) ++ "\n")
    othersreturn <- runFiles files
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runFiles [] = return 0

main :: IO ()
main = do
  args <- getArgs
  retval <- case args of
    [] -> do
      putStr "ERROR: this program expects one or more elf files as command-line arguments\n"
      return 1
    [file] -> runFile file
    files -> runFiles files
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ (fromIntegral:: Int64 -> Int) retval)
