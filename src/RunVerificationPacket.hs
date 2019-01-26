{-# LANGUAGE FlexibleContexts #-}
module RunVerificationPacket where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Data.Bits
import Data.Maybe
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
import qualified Data.Map.Strict as S
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
  let c = VerifMinimal64 { registers = S.empty,
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
                      fromHost = Nothing,
                      valid_timer = False,
                      timer = 0,
                      mem = MapMemory { bytes = mem, reservation = Nothing } } in do
    handle <- openFile "/tmp/hs2tandem" ReadWriteMode
    iterate handle (fromMaybe 0 maybeToHostAddress) c
  where 
        iterate handle toHostAddress lastState = do
               (_,nextState) <- runStateT ((cycleAndOutput toHostAddress):: IOState VerifMinimal64 ()) lastState 
               if pc lastState /= 0x80000088 then do
                  (if (fromHost nextState == Nothing) then do
                     hPutStrLn handle "s" 
                     hPutStrLn handle . show  $ pcPacket nextState
                     hPutStrLn handle . show  $ instruction nextState
                     hPutStrLn handle . show . fromEnum $ exception nextState
                     hPutStrLn handle . show . fromEnum $ interrupt nextState
                     hPutStrLn handle . show  $ cause nextState
                     hPutStrLn handle . show  $ addr nextState
                     hPutStrLn handle . show . fromEnum $ valid_addr nextState
                     hPutStrLn handle . show  $ d nextState
                     hPutStrLn handle . show . fromEnum $ valid_dst nextState
                     hPutStrLn handle . show  $ dst nextState
                     hPutStrLn handle . show  . fromEnum $valid_timer nextState
                     hPutStrLn handle . show  $ timer nextState
                     hPutStrLn handle "e"
                     iterate handle toHostAddress (nextState{valid_dst=False, valid_addr=False, valid_timer=False})
                  else do
                     putStrLn "finish" 
                     return (fromMaybe 0 (fromHost nextState)))
               else do
                putStrLn "finish"
                return .fromMaybe 0 $(S.lookup 10 $ registers nextState)  --TODO here we should get register 10 instead
        checkInterrupt = do
             mtime <- fmap (fromIntegral:: MachineInt -> Int32) (getCSRField Field.MCycle)
             mtimecmp <- loadWord mtimecmp_addr :: IOState VerifMinimal64 Int32 
             setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
             -- Check for interrupts before updating PC.
             mie <- getCSRField Field.MIE
             meie <- getCSRField Field.MEIE
             meip <- getCSRField Field.MEIP
             mtie <- getCSRField Field.MTIE
             mtip <- getCSRField Field.MTIP
             s <- get   
             let nPC = nextPC s
             k <- if (mie > 0 && ((meie > 0 && meip > 0) || (mtie > 0 && mtip > 0))) then do
                       -- Disable interrupts
                       setCSRField Field.MIE 0
                       interruptHappen <- if (meie > 0 && meip > 0) then do
                               -- Remove pending external interrupt
                               setCSRField Field.MEIP 0
                               runMaybeT (raiseException 1 11)-- Machine external interrupt.
                               return True
                             else if (mtie > 0 && mtip > 0) then do
                               runMaybeT (raiseException 1 7) -- Machine timer interrupt.
                               return True
                             else return False
                       -- Save the PC of the next (unexecuted) instruction.
                       setCSRField Field.MEPC nPC
                       return interruptHappen
                  else return False
             if (k) then 
                  put (s{pc=nextPC s}) --state $ \comp -> ((), comp{pc=nextPC comp})
             else 
                  return () 
        cycleAndOutput toHostAddress = do
               checkInterrupt
               vpc <- getPC
               s <- get
               put (s{pcPacket = vpc})
               result <- runMaybeT (runCycle RV64IMAF preDecode (endCycle)) 
               case result of
                 _ -> commit >> (postCommit toHostAddress :: IOState VerifMinimal64 ())
        preDecode inst = do 
             s <- lift get
             lift $ put (s{instruction = inst})
             return (inst /= 0x6f)
        postCommit toHostAddress = do
             s <- get 
             mtval <- getCSR MTVec
             stval <- getCSR STVec
             toHost <- loadWord toHostAddress --Finish by putting Just reg 10 in the fromHost of the state
             npc <- getPC 
             --mieg <- getCSR MIE
             --trace (show npc ++ " at pc mieg is:" ++ show mieg) $ return()
             let trappedM = npc == (mtval .&. (complement 3)) -- && ! 3
             let trappedS = npc == (stval .&. (complement 3)) -- && ! 3
             if trappedM then do
               isInterruptM <- getCSRField Field.MCauseInterrupt
               let isInterrupt = isInterruptM == 1 
               codeM <- getCSRField Field.MCauseCode
               put (s{exception= not isInterrupt, interrupt=isInterrupt, cause=codeM})
             else if trappedS then do
                   isInterruptS <- getCSRField Field.SCauseInterrupt
                   let isInterrupt = isInterruptS == 1
                   codeS <- getCSRField Field.SCauseCode
                   put (s{exception= not isInterrupt, interrupt=isInterrupt, cause=codeS})
                 else do
                   put (s{exception= False, interrupt=False, cause=0})


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
