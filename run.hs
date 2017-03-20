import System.IO
import System.Environment
import Data.Int
import Data.Word
import Data.Maybe
import Utility
import Program
import Computer32
import Decode
import Execute
import Debug.Trace
import Numeric

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
  -- tmp <- mapM getRegister [1..31]
  -- trace (showHex (fromIntegral pc) "") (return ())
  -- trace (show (decode $ fromIntegral inst)) (return ())
  -- trace (show $ map fromIntegral tmp) (return ())
  if inst == 0x6f -- Stop on infinite loop instruction.
    then getRegister 10
    else do
    setPC (pc + 4)
    execute (decode $ fromIntegral inst)
    step
    helper

runProgram :: Computer32 -> Int32
runProgram = fst . fromJust . runState helper

main :: IO ()
main = do
  a <- getArgs
  h <- openFile (head a) ReadMode
  m <- readELF h []
  let c = Computer32 { registers = (take 31 $ repeat 0), pc = 0x200, nextPC = 0, mem = (m ++ (take 65520 $ repeat (0::Word8))) } in
    putStrLn ("Return value: " ++ (show $ runProgram c))
