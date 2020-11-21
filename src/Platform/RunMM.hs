module Platform.RunMM where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Utility.Utility
import Spec.Machine
import Platform.MemoryModelTracking
import Utility.Elf
import qualified Spec.CSRField as Field
import Spec.CSRFile
import Spec.Decode
import Spec.Execute
import Spec.Spec
import Utility.MapMemory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Map.Strict as S
import qualified Data.ByteString as B
import Debug.Trace
import Numeric (showHex, readHex)
--
--processLine :: String -> (Int, [(Int, Word8)]) -> (Int, [(Int, Word8)])
--processLine ('@':xs) (p, l) = ((fst $ head $ readHex xs) * 4, l)
--processLine s (p, l) = (p + 4, l ++ (zip [p..] $ splitWord (fst $ head $ readHex s :: Word32)))
--
--readHexFile :: FilePath -> IO [(Int, Word8)]
--readHexFile f = do
--  h <- openFile f ReadMode
--  helper h (0, [])
--  where helper h l = do
--          s <- hGetLine h
--          done <- hIsEOF h
--          if (null s)
--            then return $ snd l
--            else if done
--                 then return $ snd $ processLine s l
--                 else helper h (processLine s l)
--
--
--helper preDecode preCommit = do
--    result <- runMaybeT (runCycle RV64I preDecode endCycle)
--    case result of
--      Nothing -> preCommit >> commit >> helper preDecode preCommit
--      Just _ -> return ()
-- 
--
--
--runProgram :: Minimal64 -> IO (Int64, Minimal64)
--runProgram c =
--  runStateT (helper (\inst -> 
--	let decoded = decode RV64I inst in
--	case decoded of
--	| IInstruction i -> return False 
--	| _ -> return True) endCycle) c 
--
--
--readProgram :: String -> IO (Maybe Int64, [(Int, Word8)])
--readProgram f = do
--  if ".hex" `isSuffixOf` f
--    then do
--    mem <- readHexFile f
--    return (Nothing, mem)
--    else do
--    mem <- readElf f
--    maybeToHostAddress <- readElfSymbol "tohost" f
--    return (fmap (fromIntegral:: Word64 -> Int64) maybeToHostAddress, mem)
--
--runFile :: String -> IO Int64
--runFile f = do
--  (maybeToHostAddress, program) <- readProgram f
--  let mem = S.fromList program
--  let c = Minimal64 { registers = S.empty,
--                      csrs = (resetCSRFile 64),
--                      pc = 0x0,
--                      nextPC = 0,
--                      privMode = Machine,
--                      mem = MapMemory { bytes = mem, reservation = Nothing }} in
--    fmap fst $ runProgram maybeToHostAddress c
--
main :: IO ()
main = do
	putStrLn "Tiny test"
	readerRead mp 
        readerRead mpRev 
        readerRead mpFence
        readerRead sbData 
        readerRead sbDataReb 
	return () 
  --  do
  --args <- getArgs
  --retval <- case args of
  --  [file] -> runFile file
  --  _ -> do
  --    putStr "ERROR: this program expects exactly one elf files as command-line arguments\n"
  --    return 1
  --exitWith (if retval == 0 then ExitSuccess else ExitFailure $ (fromIntegral:: Int64 -> Int) retval)
--
