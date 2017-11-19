module RunMinimal (readHex, runProgram, runFile) where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.Word
import Data.Maybe
import Utility
import Program
import Minimal64
import CSR (defaultCSRs)
import Decode
import Execute
import Numeric
import Debug.Trace
import qualified Data.Map as S

processLine :: String -> [Word8] -> [Word8]
processLine ('@':xs) l = l ++ take (4*(read ("0x" ++ xs) :: Int) - (length l)) (repeat 0)
processLine s l = l ++ splitWord (read ("0x" ++ s) :: Int32)

readHexFile :: Handle -> [Word8] -> IO [Word8]
readHexFile h l = do
  s <- hGetLine h
  done <- hIsEOF h
  if (null s)
    then return l
    else if done
         then return $ processLine s l
         else readHexFile h (processLine s l)

helper :: (RiscvProgram p t u) => p t
helper = do
  pc <- getPC
  inst <- loadWord pc
  if inst == 0x6f -- Stop on infinite loop instruction.
    then getRegister 10
    else do
    setPC (pc + 4)
    execute (decode $ fromIntegral inst)
    step
    helper

runProgram :: Minimal64 -> (Int64, Minimal64)
runProgram = fromJust . runState helper

runFile :: String -> IO Int64
runFile f = do
  h <- openFile f ReadMode
  m <- readHexFile h []
  let c = Minimal64 { registers = (take 31 $ repeat 0), pc = 0x200, nextPC = 0,
                      mem = (m ++ (take (65520 - length m) $ repeat (0::Word8))) } in
    return $ fst $ runProgram c

main :: IO ()
main = do
  file:_ <- getArgs
  retval <- runFile file
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ fromIntegral retval)
