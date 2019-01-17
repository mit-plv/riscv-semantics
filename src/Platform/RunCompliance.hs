module Platform.RunCompliance where
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.List
import Data.Word
import Utility.Utility
import Spec.Machine
import Platform.Minimal32
import Platform.MMIO
import Utility.Elf
import qualified Spec.CSRField as Field
import Spec.CSRFile
import Spec.Decode
import Spec.Execute
import Spec.VirtualMemory
import Spec.Spec
import Utility.MapMemory
import qualified Spec.Memory as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Map as S
import qualified Data.ByteString as B
import Debug.Trace
import Numeric (showHex, readHex)
import Text.Printf

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

runProgram :: Maybe Int32 -> Minimal32 -> IO (Int32, Minimal32)

runProgram maybeToHostAddress = runStateT (stepHelper RV32IM maybeToHostAddress (do
    --                                   newChar <- lift getChar
    --                                   vpc <- getPC
    --                                   lift. putStrLn . printf "%08x" $ vpc 
    --                                   pc <- translate Instruction 4 vpc
    --                                   inst <- loadWord pc
    --                                   let dinst = decode RV32IM $ fromIntegral inst 
    --                                   lift . putStrLn . show $ dinst
    --                                   l <- sequence $ fmap getRegister [1..31]
    --                                   lift . putStrLn . concat . fmap (printf "%08x ") $ l
    --                                   statem <- get
    --                                   lift . putStrLn . show $ csrs statem
                                       instret <- getCSRField Field.MInstRet
                                       if (instret > 5000)
                                         then liftIO . exitWith .  ExitFailure $ 1
                                         else liftIO checkInterrupt) :: IOState Minimal32 Int32)
-- Small MMIO hack to step each cycle



                                          

readProgram :: String -> IO (Maybe Int32, [(Int, Word8)])
readProgram f = do
  if ".hex" `isSuffixOf` f
    then do
    mem <- readHexFile f
    return (Nothing, mem)
    else do
    mem <- readElf f
    maybeToHostAddress <- readElfSymbol "tohost" f
    return (fmap (fromIntegral:: Word64 -> Int32) maybeToHostAddress, mem)

runFile :: String -> String -> String ->IO Int32
runFile f dumped dt = do
  writeFile dumped  ""
  deviceTree <- B.readFile dt 
  (maybeToHostAddress, program) <- readProgram f
  let mem = S.union (S.fromList (zip [0..] (B.unpack deviceTree))) (S.fromList program)
  let c = Minimal32 { registers = (take 31 $ repeat 0),
                      csrs = (resetCSRFile 64),
                      pc = 0x80000000,
                      nextPC = 0,
                      privMode = Machine,
                      mem = MapMemory { bytes = mem, reservation = Nothing } } in
    do
      (retValue, finalState) <- runProgram maybeToHostAddress c
      beginSignature <- readElfSymbol "begin_signature" f 
      case beginSignature of
        Nothing -> do
                putStr "ERROR: no begin-signature found"
                return 1
        Just beginSignature -> do
           let endSignature = maximum $ fmap fst program
           let signature = fmap  (M.loadWord (Platform.Minimal32.mem finalState :: MapMemory Int )) [i | i <- [(fromIntegral beginSignature :: Int) .. endSignature] , (i `mod` 4) == 0 ]
           appendFile dumped .(++"\n") . concat . (intersperse "\n")  $ fmap (\x -> printf "%08x" x) signature 
           return retValue
    -- Dump the memory starting at address <begin_signature> until end of elf file One 32 bit words per line  


main :: IO ()
main = do
  args <- getArgs
  retval <- case args of
    [] -> do
      putStr "ERROR: this program expects first the device tree, then one file to dump the signature and one elf file as command-line arguments\n"
      return 1
    [devicetree, dumped, file] -> runFile file dumped devicetree
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ (fromIntegral:: Int32 -> Int) retval)
