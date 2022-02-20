import System.IO
import Text.ParserCombinators.Parsec
import System.Environment
import qualified Data.Set as Set
import Debug.Trace as T

newtype Register = Reg String
newtype ThreadNum = TN Integer

data InitVal = Literal Integer | SymPtr String deriving Show
newtype LitmusInit = LI (ThreadNum, Register, InitVal)
instance Show LitmusInit where
  show (LI (TN t, Reg r, v)) = "LI " ++ show t ++ ":" ++ r ++ "=" ++ show v ++ ";"
newtype LitmusThread = LThread String -- just the list of instructions separated by \n
instance Show LitmusThread where
  show (LThread s) = "LT " ++ show s
newtype LitmusFile = LFile ([LitmusInit], (LitmusThread, LitmusThread)) 
instance Show LitmusFile where
  show (LFile (inits, (t0, t1))) = "L (" ++ show inits ++ ", (" ++ show t0 ++ ", " ++ show t1++ "))"

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
  return $ LI (TN thread, Reg regName, regVal)

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
  return ("  " ++ t0Line, "  " ++ t1Line)

parseThreads :: Parser (LitmusThread, LitmusThread)
parseThreads = do
  parseThreadLine -- the " P0    | P1   ;" line
  ts <- many1 parseThreadLine
  let (t0s,t1s) = unzip ts
  return (LThread $ unlines t0s, LThread $ unlines t1s)

parseLitmusFile :: Parser LitmusFile
parseLitmusFile = do
  inits <- parseInits
  threads <- parseThreads
  return $ LFile (inits, threads)

{- readaFile filename = do
  litmusFile <- openFile filename ReadMode
  hGetContents litmusFile -}

compileSymPtrs :: [LitmusInit] -> String
compileSymPtrs inits = 
  let symPtrs = foldr helper Set.empty inits in
    Set.foldr printer "" symPtrs
  where 
    helper (LI (_, _, Literal _)) ptrs = ptrs
    helper (LI (_, _, SymPtr p))  ptrs = Set.insert p ptrs
    printer sp str = sp ++ ":\n  .word 0x0\n" ++ str

compileThreadInits :: [LitmusInit] -> (String, String)
compileThreadInits = foldr helper ("", "") where
  helper (LI (TN 0, r, n)) (t0s, t1s) = (t0s ++ printIns r n, t1s)
  helper (LI (TN 1, r, n)) (t0s, t1s) = (t0s, t1s ++ printIns r n)  
  helper (LI (TN n, _, _)) p = T.trace ("found thread id " ++ show n ++ " >= 2") p -- should be impossible 
  printIns (Reg r) (Literal n) = "  li " ++ r ++ ", " ++ show n ++ "\n"
  printIns (Reg r) (SymPtr p)  = "  la " ++ r ++ ", " ++ p ++ "\n"

compileInits :: [LitmusInit] -> (String, String, String)
compileInits inits = 
  let (t0inits, t1inits) = compileThreadInits inits in
    (t0inits, t1inits, compileSymPtrs inits)

preamble :: String
preamble = "\
  \#include \"encoding.h\"\n\
  \\n\
  \# define LREG lw\n\
  \# define SREG sw\n\
  \# define REGBYTES 4\n\
  \\n\
  \.section \".text.init\"\n\
  \#  .text\n\
  \#  .align 6\n\
  \  .globl _startTh0\n\
  \  .globl _startTh1\n\
  \  .globl _endTh0\n\
  \  .globl _endTh1\n\
  \"

compileLitmusFile :: LitmusFile -> String
compileLitmusFile (LFile (inits, (LThread t0, LThread t1))) = 
  let (t0inits, t1inits, symptrs) = compileInits inits in
    preamble 
    ++ "_startTh0:\n"
    ++ t0inits
    ++ t0
    ++ "_endTh0:\n  j _endTh0\n"
    ++ "_startTh1:\n"
    ++ t1inits
    ++ t1
    ++ "_endTh1:\n  j _endTh1\n"
    ++ "\n\n"
    ++ symptrs

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
    Right lfile -> do
      print lfile
      let asmStr = compileLitmusFile lfile
      putStrLn asmStr
      writeFile (filename ++ ".S") asmStr
  hClose litmusFile
  return ()


-- TODOS:
-- - parse postconditions