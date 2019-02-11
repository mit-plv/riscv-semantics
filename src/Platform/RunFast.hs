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
import Utility.Utility
import Spec.Machine
import Platform.CleanTest
import Utility.Elf
import qualified Spec.CSRField as Field
import Spec.CSRFileIO
import Spec.Decode
import Spec.Spec
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Numeric (readHex)
import Platform.Pty
import Control.Concurrent.MVar

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

checkExternalInterrupt :: IO Bool
checkExternalInterrupt = do
  return False

runProgram :: Maybe Int64 -> VerifMinimal64 -> IO (Int64, VerifMinimal64)
runProgram maybeToHostAddress c =
  runStateT (stepHelper RV64IMAF maybeToHostAddress (do
                                                        liftIO checkExternalInterrupt) mtimecmp_addr ) c 



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
  putStrLn "init PTY"
  console <- initPty
  writeArray csrs Field.MXL 2
  writeArray csrs Field.Extensions $! encodeExtensions "IAMSU" 
  putStrLn "All the state is created"
  -- write the program and the device tree in it
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
                      console = console,
                      reservation = reservation } in
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
