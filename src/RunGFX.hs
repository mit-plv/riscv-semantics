{-# LANGUAGE NamedFieldPuns #-}
import System.IO
import System.Environment
import System.Exit
import Data.Int
import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Utility
import Program
import MMGFX32
import CSR (defaultCSRs)
import Decode
import Execute
import Debug.Trace
import Numeric
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified SDL
import SDL.Vect

processLine :: String -> [Word8] -> [Word8]
processLine ('@':xs) l = l ++ take (4*(read ("0x" ++ xs) :: Int) - (length l)) (repeat 0)
processLine s l = l ++ splitWord (read ("0x" ++ s) :: Word32)

readELF :: Handle -> [Word8] -> IO [Word8]
readELF h l = do
  s <- hGetLine h
  done <- hIsEOF h
  if (null s)
    then return l
    else if done
         then return $ processLine s l
         else readELF h (processLine s l)

data Graphics = Graphics { width :: Int, height :: Int, texture :: SDL.Texture, renderer :: SDL.Renderer }

createGraphics :: Int -> Int -> String -> IO Graphics
createGraphics width height title = do
  SDL.initializeAll
  window <- SDL.createWindow (T.pack title) SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  texture <- SDL.createTexture renderer SDL.RGB332 SDL.TextureAccessStatic (V2 (fromIntegral width) (fromIntegral height))
  return $ Graphics { width, height, texture, renderer }

updateGraphics :: Graphics -> BS.ByteString -> IO ()
updateGraphics Graphics {width, height = _, texture, renderer} pixels = do
  _ <- SDL.updateTexture texture Nothing pixels (fromIntegral $ bytesPerPixel * width)
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer

-- TODO: Find if there's a better way to lift this.
updateWrapper :: Graphics -> BS.ByteString -> MState MMGFX32 ()
updateWrapper gfx pixels = MState $ \comp -> liftIO (updateGraphics gfx pixels) >> return ((), comp)

helper :: Graphics -> MState MMGFX32 Int32
helper gfx = do
  pc <- getPC
  inst <- loadWord pc
  if inst == 0x6f -- Stop on infinite loop instruction.
    then getRegister 10
    else do
    setPC (pc + 4)
    execute (decode $ fromIntegral inst)
    step
    pixels <- getPixels
    updateWrapper gfx pixels
    helper gfx

runProgram :: MMGFX32 -> IO (Int32, MMGFX32)
runProgram comp = do
  gfx <- createGraphics screenWidth screenHeight "RISC-V emulator - now with graphics!"
  result <- (runMaybeT . (runState $ helper gfx)) comp
  return (fromJust result)

runFile :: String -> IO Int32
runFile f = do
  h <- openFile f ReadMode
  m <- readELF h []
  let c = MMGFX32 { registers = (take 31 $ repeat 0), csrs = defaultCSRs, pc = 0x200,nextPC = 0,
                   mem = (m ++ (take (65520 - length m) $ repeat (0::Word8))), mmio = baseMMIO } in
    fmap fst $ runProgram c

main :: IO ()
main = do
  file:_ <- getArgs
  retval <- runFile file
  exitWith (if retval == 0 then ExitSuccess else ExitFailure $ fromIntegral retval)
