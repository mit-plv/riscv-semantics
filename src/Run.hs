module Main where
import System.IO
import System.Environment
import System.Exit
import Data.Int
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
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as S
import Debug.Trace
import Numeric (showHex)

processLine :: String -> [Word8] -> [Word8]
processLine ('@':xs) l = l ++ take (4*(read ("0x" ++ xs) :: Int) - (length l)) (repeat 0)
processLine s l = l ++ splitWord (read ("0x" ++ s) :: Word32)

readHexFile :: Handle -> [Word8] -> IO [Word8]
readHexFile h l = do
  s <- hGetLine h
  done <- hIsEOF h
  if (null s)
    then return l
    else if done
         then return $ processLine s l
         else readHexFile h (processLine s l)

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

helper :: Maybe Int64 -> IOState Minimal64 Int64
helper maybeToHostAddress = do
  toHostValue <- case maybeToHostAddress of
    Nothing -> return 0 -- default value
    Just toHostAddress -> loadWord toHostAddress
  if toHostValue /= 0
    then do
      -- quit running
      if toHostValue == 1
        then trace "PASSED" (return 0)
        else trace ("FAILED " ++ (show $ quot toHostValue 2)) (return 1)
    else do
      pc <- getPC
      -- trace ("pc: 0x" ++ (showHex pc "")) $ return ()
      inst <- loadWord pc
      if inst == 0x6f -- Stop on infinite loop instruction.
        then do
            cycles <- getCSRField Field.MCycle
            trace ("Cycles: " ++ show cycles) (return ())
            instret <- getCSRField Field.MInstRet
            trace ("Insts: " ++ show instret) (return ())
            getRegister 10
        else do
        setPC (pc + 4)
        size <- getXLEN
        execute (decode size $ fromIntegral inst)
        interrupt <- liftIO checkInterrupt
        if interrupt then do
          -- Signal interrupt by setting MEIP high.
          setCSRField Field.MEIP 1
        else return ()
        step
        helper maybeToHostAddress

runProgram :: Maybe Int64 -> Minimal64 -> IO (Int64, Minimal64)
runProgram maybeToHostAddress = runStateT (helper maybeToHostAddress)

runFile :: String -> IO Int64
runFile f = do
  h <- openFile f ReadMode
  m <- readHexFile h []
  let c = Minimal64 { registers = (take 31 $ repeat 0), csrs = (resetCSRFile 64), pc = 0x200, nextPC = 0,
                      mem = S.fromList $ zip [0..] (m ++ (take (65520 - length m) $ repeat (0::Word8))) } in
    fmap fst $ runProgram Nothing c

runElf :: String -> IO Int64
runElf f = do
  m <- readElf f
  -- converts Word32 to Int32
  maybeToHostAddress <- fmap (fmap (fromInteger . toInteger)) $ readElfSymbol "tohost" f
  let c = Minimal64 { registers = (take 31 $ repeat 0), csrs = (resetCSRFile 64), pc = 0x80000000, nextPC = 0,
                      mem = S.fromList m } in
    fmap fst $ runProgram maybeToHostAddress c

runElfs :: [String] -> IO Int64
runElfs (file:files) = do
    myreturn <- runElf file
    putStr (file ++ ": " ++ (show myreturn) ++ "\n")
    othersreturn <- runElfs files
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runElfs [] = return 0

main :: IO ()
main = do
  args <- getArgs
  retval <- case args of
    [] -> do
      putStr "ERROR: this program expects one or more elf files as command-line arguments\n"
      return 1
    [file] -> runElf file
    files -> runElfs files
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ fromIntegral retval)
