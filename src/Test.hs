module Test where
import Data.Int
import qualified Data.Map as S
import Control.Monad.State
import System.Exit

import Minimal64
import Decode
import Execute
import Run (readProgram)
import BufferMMIO
import CSRFile
import Program

data Test = Test { name :: String, input :: String, returnValue :: Int64, output :: String }

runTest :: Test -> IO Bool
runTest (Test name input returnValue output) = do
  result <- runFile ("test/build/" ++ name ++ "64") input
  return $ result == (returnValue, output)

-- TODO: Read this from a file.
tests :: [Test]
tests = [Test "add" ""  11 "",
         Test "sub" ""   7 "",
         Test "mul" ""  42 "",
         Test "and" ""  35 "",
         Test "or"  "" 111 "",
         Test "xor" ""  76 "",
         Test "csr" ""  29 "",
         Test "hello" "" 0 "Hello, world!\n",
         Test "reverse" "asdf" 0 "fdsa\n",
         Test "thuemorse" "" 0 "01101001100101101001011001101001100101100110100101101001100101101001011001101001011010011001011001101001100101101001011001101001\n",
         Test "illegal" "" 0 "!\n?\n",
         Test "time" "" 0 "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\n!\n.\n"]

-- TODO: Reduce code duplication with Run.
helper :: Maybe Int64 -> BufferState Minimal64 Int64
helper maybeToHostAddress = do
  toHostValue <- case maybeToHostAddress of
    Nothing -> return 0 -- default value
    Just toHostAddress -> loadWord toHostAddress
  if toHostValue /= 0
    then do
      -- quit running
      if toHostValue == 1
        then return 0
        else return 1
    else do
      pc <- getPC
      inst <- loadWord pc
      if inst == 0x6f -- Stop on infinite loop instruction.
        then getRegister 10
        else do
        setPC (pc + 4)
        size <- getXLEN
        execute (decode size $ fromIntegral inst)
        step
        helper maybeToHostAddress

runProgram :: Maybe Int64 -> Minimal64 -> String -> (Int64, String)
runProgram maybeToHostAddress comp input = (returnValue, output)
  where ((returnValue, _), output) = runBufferIO (runStateT (helper maybeToHostAddress) comp) input

runFile :: String -> String -> IO (Int64, String)
runFile f input = do
  (maybeToHostAddress, mem) <- readProgram f
  let c = Minimal64 { registers = (take 31 $ repeat 0), csrs = (resetCSRFile 64), pc = 0x80000000, nextPC = 0,
                      mem = S.fromList mem } in
    return $ runProgram maybeToHostAddress c input

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
