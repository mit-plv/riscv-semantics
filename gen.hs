import qualified Data.Text as T
import Data.List
import Data.Char
import Data.List.Split

removeComments :: [String] -> [String]
removeComments = filter (\s -> length s > 2 && s !! 0 /= '#')

splitClean :: String -> [String]
splitClean line = let repl '.' = '_'
                      repl c = c
                  in filter (not . (isSuffixOf "=ignore")) $ words $ map repl line

genType :: String -> String
genType line = let pieces = splitClean line
                   args = takeWhile (not . isDigit . head) (tail pieces)
                   fields = map (++ " :: Int32") args
                   opcode = (toUpper $ head $ head pieces):(tail $ head pieces)
               in
                if length fields > 0 then
                  opcode ++ " { " ++ intercalate ", " fields ++ " }"
                else
                  opcode

parseRange :: String -> (Int, Int, String)
parseRange range = let pieces = splitOn "=" range
                       startEnd = map (read::String -> Int) $ splitOn "__" $ pieces !! 0
                   in (startEnd !! 1, startEnd !! 0 + 1, pieces !! 1)

genEntry :: String -> String
genEntry line = let pieces = splitClean line
                    ranges = map parseRange $ filter (isDigit . head) (tail pieces)
                    opcode = (toUpper $ head $ head pieces):(tail $ head pieces)
                in "(decode" ++ opcode ++ ", " ++ (filter (/= '"') (show ranges)) ++ ")"

main :: IO ()
main = do
  xs <- readFile "opcodes" >>= (return . removeComments . lines)
  putStrLn (intercalate " |\n" $ map genType xs)
  putStrLn (intercalate ", \n" $ map genEntry xs)
  
