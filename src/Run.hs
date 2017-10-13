module Main where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.Word
import Data.Maybe
import Utility
import Program
import MMIO64
import CSR hiding (decode)
import Decode
import Execute
import Numeric
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Map as S

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

checkInterrupt :: IO Bool
checkInterrupt = do
  ready <- hReady stdin
  if ready then do
    c <- hLookAhead stdin
    if c == '!' then do
      getChar
      getChar
      return True
    else return False
  else return False

helper :: MState MMIO64 Int64
helper = do
  pc <- getPC
  inst <- loadWord pc
  if inst == 0x6f -- Stop on infinite loop instruction.
    then getRegister 10
    else do
    setPC (pc + 4)
    execute (decode $ fromIntegral inst)
    interrupt <- (MState $ \comp -> liftIO checkInterrupt >>= (\b -> return (b, comp)))
    if interrupt then do
      -- Signal interrupt by setting MEIP high.
      mip <- loadCSR mip_addr
      storeCSR mip_addr (encode ((decodeMIP mip) { meip = True }))
    else return ()
    step
    helper

runProgram :: MMIO64 -> IO (Int64, MMIO64)
runProgram = (fmap fromJust) . runMaybeT . (runState helper)

runFile :: String -> IO Int64
runFile f = do
  h <- openFile f ReadMode
  m <- readELF h []
  let c = MMIO64 { registers = (take 31 $ repeat 0), csrs = defaultCSRs, pc = 0x200, nextPC = 0,
                   mem = S.fromList $ zip [0..] (m ++ (take (65520 - length m) $ repeat (0::Word8))), mmio = baseMMIO } in
    fmap fst $ runProgram c

main :: IO ()
main = do
  file:_ <- getArgs
  retval <- runFile file
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ fromIntegral retval)
