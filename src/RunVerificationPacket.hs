{-# LANGUAGE FlexibleContexts #-}
module RunVerificationPacket where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Utility
import Program
import VerifMinimal64
import MMIO
import Elf
import qualified CSRField as Field
import CSRFile
import CSRSpec
import CSR
import Decode
import Execute
import VirtualMemory
import Spec
import MapMemory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Map as S
import qualified Data.ByteString as B
import Debug.Trace
import Numeric (showHex, readHex)

processLine :: String -> (Int, [(Int, Word8)]) -> (Int, [(Int, Word8)])
processLine ('@':xs) (p, l) = ((fst $ head $ readHex xs) * 4, l)
processLine s (p, l) = (p + 4, l ++ (zip [p..] $ splitWord (fst $ head $ readHex s :: Word32)))

readHexFile :: FilePath -> IO [(Int, Word8)]
readHexFile f = do
  h <- openFile f ReadMode
  helper h (0, [])
  where helper h l = do
          s <- hGetLine h
          done <- hIsEOF h
          if (null s)
            then return $ snd l
            else if done
                 then return $ snd $ processLine s l
                 else helper h (processLine s l)

checkInterrupt :: IO Bool
checkInterrupt = do
  ready <- hReady stdin
  if ready then do
    c <- hLookAhead stdin
    if c == '!' then do
      _ <- getChar
      _ <- getChar
      return True
    else return False
  else return False

runProgram :: Maybe Int64 -> VerifMinimal64 -> IO (Int64, VerifMinimal64)

runProgram maybeToHostAddress = runStateT (stepHelper RV64IMAF maybeToHostAddress (liftIO checkInterrupt) :: IOState VerifMinimal64 Int64)

readProgram :: String -> IO (Maybe Int64, [(Int, Word8)])
readProgram f = do
  if ".hex" `isSuffixOf` f
    then do
    mem <- readHexFile f
    return (Nothing, mem)
    else do
    mem <- readElf f
    maybeToHostAddress <- readElfSymbol "tohost" f
    return (fmap (fromIntegral:: Word64 -> Int64) maybeToHostAddress, mem)

runFile :: String -> IO Int64
runFile f = do
  deviceTree <- B.readFile "src/device_tree.bin"
  (maybeToHostAddress, program) <- readProgram f
  let mem = S.union (S.fromList (zip [0..] (B.unpack deviceTree))) (S.fromList program)
  let c = VerifMinimal64 { registers = (take 31 $ repeat 0),
                      fpregisters = (take 32 $ repeat 0),
                      csrs = (resetCSRFile 64),
                      pc = 0x80000000,
                      addr = 0,
                      valid_addr = False,
                      exception = False,
                      interrupt = False,
                      cause = 0,
                      d = 0,
                      valid_dst = False,
                      dst = 0,
                      nextPC = 0,
                      privMode = Machine,
                      mem = MapMemory { bytes = mem, reservation = Nothing } } in
    iterate c
  where 
        iterate lastState = do
               nextState <- fmap snd $ runStateT (cycleAndOutput :: IOState VerifMinimal64 ()) lastState 
               if pc lastState /= 0x6f then do
--                  line <- getLine
                  let line = "n" 
                  (if (line == "n") then do
                     putStrLn "s" 
                     putStrLn . show  $ pcPacket nextState
                     putStrLn . show  $ instruction nextState
                     putStrLn . show . fromEnum $ exception nextState
                     putStrLn . show . fromEnum $ interrupt nextState
                     putStrLn . show  $ cause nextState
                     putStrLn . show  $ addr nextState
                     putStrLn . show . fromEnum $ valid_addr nextState
                     putStrLn . show  $ d nextState
                     putStrLn . show . fromEnum $ valid_dst nextState
                     putStrLn . show  $ dst nextState
                     putStrLn "e"
                     iterate (nextState{valid_dst=False, valid_addr=False})
                  else do
                     putStrLn "failure" 
                     return 0)
               else do
                putStrLn "finish"
                return 0
        cycleAndOutput = do
               result <- runMaybeT (runCycle RV64IMAF preDecode preCommit) 
               case result of
                 Nothing -> commit 
                 Just _ -> commit --We go through independently we stop at top level 
        preDecode inst = do 
             vpc <- getPC
             s <- lift get
             lift $ put (s{pcPacket = vpc, instruction = inst})
             return (inst /= 0x6f)
        preCommit = do
             s <- lift get
             mtval <- getCSR MTVal
             stval <- getCSR STVal
             let npc = nextPC s
             let trappedM = npc == (mtval .&. (-3)) -- && ! 3
             let trappedS = npc == (stval .&. (-3)) -- && ! 3
             if trappedM then do
               isInterruptM <- getCSRField Field.MCauseInterrupt
               let isInterrupt = isInterruptM == 1 
               codeM <- getCSR MCause
               lift $ put (s{exception= not isInterrupt, interrupt=isInterrupt, cause=codeM})
               endCycle
             else if trappedS then do
                 isInterruptS <- getCSRField Field.SCauseInterrupt
                 let isInterrupt = isInterruptS == 1
                 codeS <- getCSR SCause
                 lift $ put (s{exception= not isInterrupt, interrupt=isInterrupt, cause=codeS})
                 endCycle
                 else do
                   lift $ put (s{exception= False, interrupt=False, cause=0})
                   endCycle


runFiles :: [String] -> IO Int64
runFiles (file:files) = do
    myreturn <- runFile file
    putStr (file ++ ": " ++ (show myreturn) ++ "\n")
    othersreturn <- runFiles files
    if myreturn /= 0
        then return myreturn
        else return othersreturn
runFiles [] = return 0

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  retval <- case args of
    [] -> do
      putStr "ERROR: this program expects one or more elf files as command-line arguments\n"
      return 1
    [file] -> runFile file
    files -> runFiles files
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ (fromIntegral:: Int64 -> Int) retval)
