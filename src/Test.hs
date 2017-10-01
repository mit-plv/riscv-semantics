import Data.Int
import Minimal64
import Decode
import Execute
import RunMinimal

c = Minimal64 { registers = [0,0,0,0], pc = 5, nextPC = 0, mem = [0,0,0,0] }
action = do
  execute (Lui 1 19)
  execute (Lui 2 23)
  execute (Lui 4 1)
  execute (Add 3 1 2)
  execute (Sw 4 3 0)
cp = runState action c

runTest :: String -> Int64 -> IO Bool
runTest f expected = do
  result <- runFile f
  return $ result == expected

tests :: [(String, Int64)]
tests = [("add",  11),
         ("sub",   7),
         ("mul",  42),
         ("and",  35),
         ("or" , 111),
         ("xor",  76)]

main :: IO ()
main = do
  results <- mapM (\(f, x) -> runTest ("../test/tests/" ++ f ++ ".hex") x) tests
  mapM_ (putStrLn . show) (zip (map fst tests) results)
  if all id results then
    putStrLn "All tests passed!"
    else
    putStrLn $ (show (sum (map fromEnum results))) ++ "/" ++ (show $ length tests) ++ " tests passed."
