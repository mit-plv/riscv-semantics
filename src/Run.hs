module Run where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Utility
import Program
import Minimal64
import MMIO
import Elf
import qualified CSRField as Field
import CSRFile
import Decode
import Execute
import VirtualMemory
import Spec
import MapMemory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Map as S
import Debug.Trace
import Numeric (showHex, readHex)

processLine :: String -> (Int, [(Int, Word8)]) -> (Int, [(Int, Word8)])
processLine ('@':xs) (p, l) = ((fst $ head $ readHex xs) * 4, l)
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

checkInterrupt :: IO Bool
checkInterrupt = do
  ready <- hReady stdin
  if ready then do
    c <- hLookAhead stdin
    if c == '!' then do
      _ <- getChar
      _ <- getChar
      return True
    else return False
  else return False

runProgram :: Maybe Int64 -> Minimal64 -> IO (Int64, Minimal64)

runProgram maybeToHostAddress = runStateT (stepHelper RV64IMA maybeToHostAddress (liftIO checkInterrupt) :: IOState Minimal64 Int64)

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
  (maybeToHostAddress, mem) <- readProgram f
  let c = Minimal64 { registers = (take 31 $ repeat 0),
                      fpregisters = (take 32 $ repeat (0 :: Float)),
                      csrs = (resetCSRFile 64),
                      pc = 0x80000000,
                      nextPC = 0,
                      privMode = Machine,
                      mem = MapMemory { bytes = S.fromList mem, reservation = Nothing } } in
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
