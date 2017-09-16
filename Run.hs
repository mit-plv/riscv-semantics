import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.Word
import Data.Maybe
import Utility
import Program
import MMIO32
import CSR (defaultCSRs)
import Decode
import Execute
import Debug.Trace
import Numeric
import Control.Monad.Trans.Maybe
import qualified Data.Sequence as S
processLine :: String -> [Word8] -> [Word8]
processLine ('@':xs) l = l ++ take (4*(read ("0x" ++ xs) :: Int) - (length l)) (repeat 0)
processLine s l = l ++ splitWord (read ("0x" ++ s) :: Word32)

readELF :: Handle -> [Word8] -> IO [Word8]
readELF h l = do
  s <- hGetLine h
  done <- hIsEOF h
  if (null s)
    then return l
    else if done
         then return $ processLine s l
         else readELF h (processLine s l)

helper :: (RiscvProgram p t u) => p t
helper = do
  pc <- getPC
  inst <- loadWord pc
  -- trace (showHex (fromIntegral inst :: Word32) "") (return ())
  -- trace (show (decode $ fromIntegral inst)) (return ())
  if inst == 0x6f -- Stop on infinite loop instruction.
    then getRegister 10
    else do
    setPC (pc + 4)
    execute (decode $ fromIntegral inst)
    step
    helper

runProgram :: MMIO32 -> IO (Int32, MMIO32)
runProgram = (fmap fromJust) . runMaybeT . (runState helper)

runFile :: String -> IO Int32
runFile f = do
  h <- openFile f ReadMode
  m <- readELF h []
  let c = MMIO32 { registers = (take 31 $ repeat 0), csrs = defaultCSRs, pc = 0x200,nextPC = 0,
                   mem = S.fromList $(m ++ (take (65520 - length m) $ repeat (0::Word8))), mmio = baseMMIO } in
    fmap fst $ runProgram c

main :: IO ()
main = do
  file:_ <- getArgs
  retval <- runFile file
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ fromIntegral retval)
