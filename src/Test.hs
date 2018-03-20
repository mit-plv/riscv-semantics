module Test where
import Data.Int
import qualified Data.Map as S
import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.Exit

import Minimal64
import Decode
import Execute
import Run (readProgram)
import BufferMMIO
import CSRFile
import Program
import Spec
import Utility
import VirtualMemory

data Test = Test { name :: String, instructionSet :: InstructionSet, input :: String, returnValue :: Int64, output :: String }

runTest :: Test -> IO Bool
runTest (Test name iset input returnValue output) = do
  result <- runFile iset ("test/build/" ++ name ++ "64") input
  let isSuccess = (result == (returnValue, output))
  when (not isSuccess) (putStrLn ("Running " ++ name ++ " gave output " ++ show result ++ " but expected " ++ show (returnValue, output)))
  return $ isSuccess

-- TODO: Read this from a file.
tests :: [Test]
tests = [Test "add" RV64I ""  11 "",
         Test "ebreak" RV64IM "" 0 "D\n?\n",
         Test "mul_support" RV64IM "" 0 "A\n",
         Test "mul_support" RV64I  "" 0 "cA\n",
         Test "sub" RV64I ""   7 "",
         Test "mul" RV64IM ""  42 "",
         Test "and" RV64I ""  35 "",
         Test "or"  RV64IM "" 111 "",
         Test "xor" RV64I ""  76 "",
         Test "csr" RV64IM ""  29 "",
         Test "hello" RV64IM "" 0 "Hello, world!\n",
         Test "reverse" RV64IM "asdf" 0 "fdsa\n",
         Test "thuemorse" RV64IM "" 0 "01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001\n",
         Test "illegal" RV64IM "" 0 "!\n?\n",
         Test "time" RV64IM "" 0 "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\n!\n.\n"]

runProgram :: InstructionSet -> Maybe Int64 -> Minimal64 -> String -> (Int64, String)
runProgram iset maybeToHostAddress comp input = (returnValue, output)
  where ((returnValue, _), output) = runBufferIO (runStateT (stepHelper iset maybeToHostAddress (return False) :: BufferState Minimal64 Int64) comp) input

runFile :: InstructionSet -> String -> String -> IO (Int64, String)
runFile iset f input = do
  (maybeToHostAddress, mem) <- readProgram f
  let c = Minimal64 { registers = (take 31 $ repeat 0), csrs = (resetCSRFile 64), pc = 0x80000000, nextPC = 0,
                      mem = S.fromList mem } in
    return $ runProgram iset maybeToHostAddress c input

main :: IO ()
main = do
  results <- mapM runTest tests
  mapM_ (putStrLn . show) (zip (map name tests) results)
  if all id results
    then do
    putStrLn "All tests passed!"
    exitWith ExitSuccess
    else do
    putStrLn $ (show (sum (map fromEnum results))) ++ "/" ++ (show $ length tests) ++ " tests passed."
    exitWith (ExitFailure 1)
