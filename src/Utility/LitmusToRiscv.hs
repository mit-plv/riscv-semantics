import System.IO
import Text.ParserCombinators.Parsec
import System.Environment

data InitVal = Literal Integer | SymPtr String deriving Show
newtype LitmusInit = LI (Integer, String, InitVal)
instance Show LitmusInit where
  show (LI (t, n, v)) = "LI " ++ show t ++ ":" ++ n ++ "=" ++ show v ++ ";"

parseInitVal :: Parser InitVal
parseInitVal =
  (Literal . read <$> many1 digit)
  <|> (SymPtr <$> many1 lower)

parseInit :: Parser LitmusInit
parseInit = do
  thread <- read <$> many1 digit
  char ':'
  regName <- many1 alphaNum
  char '='
  regVal <- parseInitVal
  char ';'
  return $ LI (thread, regName, regVal)

parseInits :: Parser [LitmusInit]
parseInits = do
  manyTill anyChar $ try $ char '{'
  spaces
  is <- endBy parseInit spaces
  char '}'
  newline
  return is


  -- currently assuming only two threads 
parseThreadLine :: Parser (String, String)
parseThreadLine = do
  char ' '
  t0Line <- manyTill anyChar $ try $ char '|'
  char ' '
  t1Line <- manyTill anyChar $ try $ char ';'
  newline
  return (' ' : t0Line, ' ' : t1Line)

parseThreads :: Parser (String, String)
parseThreads = do
  parseThreadLine -- the " P0    | P1   ;" line
  ts <- many1 parseThreadLine
  let (t0s,t1s) = unzip ts
  return (unlines t0s, unlines t1s)

parseLitmusFile :: Parser ([LitmusInit], (String, String))
parseLitmusFile = do
  inits <- parseInits
  threads <- parseThreads
  return (inits, threads)

{- readaFile filename = do
  litmusFile <- openFile filename ReadMode
  hGetContents litmusFile -}


main :: IO ()
main = do
  -- TODO fix hardcoding of filename
  args <- getArgs
  let filename = head args
  litmusFile <- openFile filename ReadMode
  litmusStr <- hGetContents litmusFile
  --putStrLn litmusStr
  putStrLn filename
  case parse parseLitmusFile "blah" litmusStr of
    Left _ -> putStrLn "Parse error"
    Right f -> print f


--  asmFile <- openFile "/home/pratapsingh/riscv-semantics/test/litmus/SB.S" WriteMode
  return ()
