module Utility.LitmusToRiscv where
import System.IO
import Text.ParserCombinators.Parsec
import System.Process
import qualified Data.Set as Set
import Debug.Trace as T
import Data.Int ( Int64 )
import Data.Word ( Word8 )
import Spec.Decode ( Register )


newtype RegString = Reg String
newtype ThreadNum = TN Integer

data InitVal = Literal Integer | SymPtr String deriving Show
newtype LitmusInit = LI (ThreadNum, RegString, InitVal)
instance Show LitmusInit where
  show (LI (TN t, Reg r, v)) = "LI " ++ show t ++ ":" ++ r ++ "=" ++ show v ++ ";"
newtype LitmusThread = LThread String -- just the list of instructions separated by \n
instance Show LitmusThread where
  show (LThread s) = "LT " ++ show s
data SymCondition = SRegCond (Int64, Register, Int64) | SMemCond (String, Word8) deriving Show
newtype Condition = RegCond (Int64, Register, Int64) deriving Show
newtype LitmusFile = LFile ([LitmusInit], (LitmusThread, LitmusThread), [SymCondition])
instance Show LitmusFile where
  show (LFile (inits, (t0, t1), conds)) = "L (" ++ show inits ++ ", (" ++ show t0 ++ ", " ++ show t1++ ")," ++ show conds ++ ")"

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

parseCond :: Parser SymCondition
parseCond = do
  parseSRegCond <|> parseSMemCond
  where
    parseSRegCond = do
      thread <- read <$> many1 digit
      char ':'
      char 'x'
      regNum <- read <$> many1 digit
      char '='
      regVal <- read <$> many1 digit
      return $ SRegCond (thread, regNum, regVal)
    parseSMemCond = do
      ptrName <- many1 lower
      char '='
      regVal <- read <$> many1 digit
      return $ SMemCond (ptrName, regVal)

parseConds :: Parser [SymCondition]
parseConds = do
  string "exists"
  newline
  char '('
  cs <- parseCond `sepBy` string " /\\ "
  char ')'
  return cs

parseLitmusFile :: Parser LitmusFile
parseLitmusFile = do
  inits <- parseInits
  threads <- parseThreads
  conds <- parseConds
  return $ LFile (inits, threads, conds)

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
  printIns (Reg r) (Literal n) = "  li " ++ r ++ "," ++ show n ++ "\n"
  printIns (Reg r) (SymPtr p)  = "  la " ++ r ++ "," ++ p ++ "\n"

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
  \  .globl _start\n\
  \  .globl _endTh0\n\
  \  .globl _endTh1\n\
  \"

compileSMemConds :: [SymCondition] -> (String, String, [Condition])
compileSMemConds conds = 
  -- This function uses registers x18 and up to read values in memory locations
  -- For the basic 2-thread tests, only registers x5-x10 are used, so no conflicts occur
  -- More complex litmus tests may require a more sophisticated register choice strategy
  let (t0checks, t1checks, newConds, _) = foldr helper ("", "", [], 18) conds in
    (if t0checks == "" then "" else "  fence\n" ++ t0checks,
     if t1checks == "" then "" else "  fence\n" ++ t1checks,
     newConds)
  where
    helper (SRegCond r) (t0acc, t1acc, condacc, n) = (t0acc, t1acc, RegCond r : condacc, n)
    helper (SMemCond (ptr, val)) (t0acc, t1acc, condacc, n) = 
      (t0acc ++ "  lw x" ++ show n ++ "," ++ ptr ++ "\n",
       t0acc ++ "  lw x" ++ show n ++ "," ++ ptr ++ "\n",
       RegCond (0, n, fromIntegral val) : RegCond (1, n, fromIntegral val) : condacc,
       n + 1
      )

compileLitmusFile :: LitmusFile -> ([Condition], String)
compileLitmusFile (LFile (inits, (LThread t0, LThread t1), conds)) =
  let (t0inits, t1inits, symptrs) = compileInits inits in
  let (t0memChecks, t1memChecks, newConds) = compileSMemConds conds in
    (newConds, preamble
                ++ "_start:\n"
                ++ "_startTh0:\n"
                ++ t0inits
                ++ t0
                ++ t0memChecks
                ++ "_endTh0:\n  j _endTh0\n"
                ++ "_startTh1:\n"
                ++ t1inits
                ++ t1
                ++ t1memChecks
                ++ "_endTh1:\n  j _endTh1\n"
                ++ "\n\n"
                ++ symptrs
    )

litmusToRiscv :: String -> IO (String, [Condition])
litmusToRiscv filename = do
  -- TODO fix hardcoding of filename
  litmusFile <- openFile filename ReadMode
  litmusStr <- hGetContents litmusFile
  --putStrLn litmusStr
  --putStrLn filename
  case parse parseLitmusFile "blah" litmusStr of
    Left _ -> do
      putStrLn "Parse error"
      hClose litmusFile
      return ("", [])
    Right lfile -> do
      -- print lfile
      let (newConds, asmStr) = compileLitmusFile lfile
      putStrLn asmStr
      -- print newConds
      writeFile (filename ++ ".S") asmStr
      let callGccO = proc "riscv-none-embed-gcc" ["-march=rv64im", "-mabi=lp64", "-static", "-nostdlib", "-nostartfiles", "-mcmodel=medany", "-c", filename ++ ".S", "-o", filename ++ ".o"]
      let callGccExe = proc "riscv-none-embed-gcc" ["-march=rv64im", "-mabi=lp64", "-static", "-nostdlib", "-nostartfiles", "-mcmodel=medany", "-o", filename ++ ".exe", filename ++ ".o"]
      (exitCodeO, stdoutO, stderrO) <- readCreateProcessWithExitCode callGccO "stdin"
      -- putStrLn $ "first gcc call returned:\n" ++ show exitCodeO ++ "\n stdOut: " ++ stdoutO ++ "\n stderr:" ++ stderrO
      (exitCodeExe, stdoutExe, stderrExe) <- readCreateProcessWithExitCode callGccExe "stdin"
      -- putStrLn $ "second gcc call returned:\n" ++ show exitCodeExe ++ "\n stdOut: " ++ stdoutExe ++ "\n stderr:" ++ stderrExe
      hClose litmusFile
      return (filename ++ ".exe", newConds)
  
{- main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  litmusToRiscv filename 
  return () -}

-- TODOS:
-- - parse postconditions