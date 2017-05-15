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

capitalize :: String -> String
capitalize (x:xs) = (toUpper x):xs
capitalize "" = ""

parseOp :: String -> (String, [String])
parseOp line = let pieces = splitClean line
                   args = takeWhile (not . isDigit . head) (tail pieces)
                   op = capitalize (head pieces)
               in (op, args)

genType :: String -> String
genType line = let (opcode, args) = parseOp line
                   addType arg = arg ++ " :: " ++ (case (arg !! 0) of
                                                      'r' -> "Register"
                                                      'c' -> "Int"
                                                      _  -> "Integer")
                   fields = map addType args
               in
                if length fields > 0 then
                  opcode ++ " { " ++ intercalate ", " fields ++ " }"
                else
                  opcode

genFunction :: String -> String
genFunction line = let (opcode, args) = parseOp line
                       cargs = map (("(get" ++) . (++ " inst)") . capitalize) args
                   in "decode" ++ opcode ++ " inst = " ++ opcode ++ " " ++ unwords cargs

parseRange :: String -> (Int, Int, String)
parseRange range = let pieces = splitOn "=" range
                       startEnd = map (read::String -> Int) $ splitOn "__" $ pieces !! 0
                   in (startEnd !! 1, startEnd !! 0 + 1, pieces !! 1)

genEntry :: String -> String
genEntry line = let pieces = splitClean line
                    ranges = map parseRange $ filter (isDigit . head) (tail pieces)
                    opcode = (toUpper $ head $ head pieces):(tail $ head pieces)
                in "(decode" ++ opcode ++ ", " ++ (filter (/= '"') (show ranges)) ++ ")"

filterLines :: String -> [String]
filterLines = map snd . filter (\(i, _) -> i `elem` ([0..38] ++ [69..76] ++ [120..134])) . zip [0..] . removeComments . lines

defineInstruction :: [String] -> String
defineInstruction xs = "data Instruction =\n" ++
                       intercalate " |\n" (map (("  " ++) . genType) xs) ++
                       "\n  deriving (Eq, Read, Show)"

defineFunctions :: [String] -> String
defineFunctions = intercalate "\n" . map genFunction

-- (decodeLui, [(2, 7, 0x0D), (0, 2, 3)]) corresponds to (lui 6..2=0x0D 1..0=3) in opcodes file.
defineTable :: [String] -> String
defineTable xs = "opcodeTable :: [(Integer -> Instruction, [(Int, Int, Integer)])]\n" ++
                 "opcodeTable = [" ++ (intercalate ", \n               " $ map genEntry xs) ++ "]"

generateCode :: [String] -> String
generateCode xs = defineInstruction xs ++ "\n\n" ++
                  defineFunctions xs ++ "\n\n" ++
                  defineTable xs ++ "\n"

main :: IO ()
main = do
  template <- readFile "Decode_base.hs"
  xs <- fmap filterLines $ readFile "opcodes"
  writeFile "Decode.hs" $ template ++ generateCode xs
  
