{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module MMIO32 where
import Prelude
import Program
import Utility
import CSR
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO.Error

newtype MState s a = MState { runState :: s -> (MaybeT IO) (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> fmap (\(b,s) -> (f b, s)) (runState a state)

instance Applicative (MState s) where
  pure x = MState $ \state -> return (x, state)
  (<*>) f a = MState $ \state -> do
    (g, s1) <- runState f state
    (b, s2) <- runState a s1
    return (g b, s2)

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> runState a state >>= (\(b, s) -> runState (f b) s)

instance Alternative (MState s) where
  empty = MState $ \state -> empty
  (<|>) a b = MState $ \state -> runState a state <|> runState b state

instance MonadPlus (MState s)

data MMIO32 = MMIO32 { registers :: [Int32], csrs :: [(Int, CSR)], pc :: Int32, nextPC :: Int32, mem :: [Word8], mmio :: [(LoadFunc, StoreFunc)] }
              deriving (Show)

-- open, close, read, write

type LoadFunc = MState MMIO32 Int32
type StoreFunc = Int32 -> MState MMIO32 ()

instance (Show LoadFunc) where
  show x = "<loadfunc>"
instance (Show StoreFunc) where
  show x = "<storefunc>"

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

ord32 :: Char -> Int32
ord32 = fromIntegral . ord

chr32 :: Int32 -> Char
chr32 = chr . fromIntegral

cGetChar :: IO Int32
cGetChar= catchIOError (fmap ord32 getChar) (\e -> if isEOFError e then return (-1) else ioError e)

rvGetChar :: LoadFunc
rvGetChar = MState $ \comp -> liftIO cGetChar >>= (\c -> return (c, comp))
rvPutChar :: StoreFunc
rvPutChar val = MState $ \comp -> liftIO (putChar $ chr32 val) >> return ((), comp)

baseMMIO = [(rvGetChar, rvPutChar)]

mmioStart :: (Num a) => MMIO32 -> a
mmioStart = (+1) . fromIntegral . length . mem

instance RiscvProgram (MState MMIO32) Int32 Word32 where
  getRegister reg = MState $ \comp -> return (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> return ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte addr = MState $ \comp -> return (fromIntegral $ (mem comp) !! (fromIntegral addr), comp)
  loadHalf addr = MState $ \comp -> return (combineBytes $ take 2 $ drop (fromIntegral addr) (mem comp), comp)
  loadWord addr = MState $ \comp -> if
    | addr > (mmioStart comp) -> runState (fst $ (mmio comp) !! ((fromIntegral $ addr - mmioStart comp) `div` 4)) comp
    | otherwise -> return (combineBytes $ take 4 $ drop (fromIntegral addr) (mem comp), comp)
  storeByte addr val = MState $ \comp -> return ((), comp { mem = setIndex (fromIntegral addr) (fromIntegral val) (mem comp) })
  storeHalf addr val = MState $ \comp -> return ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitHalf val) })
  storeWord addr val = MState $ \comp -> if
    | addr > (mmioStart comp) -> runState (snd ((mmio comp) !! ((fromIntegral $ addr - mmioStart comp) `div` 4)) (fromIntegral val)) comp
    | otherwise -> return ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitWord val) })
  loadCSR addr = MState $ \comp -> return (encode $ fromJust $ lookup addr (csrs comp), comp)
  storeCSR addr val = MState $ \comp -> return ((), comp { csrs = setIndex (fromJust $ elemIndex addr (map fst $ csrs comp))
                                                                           (addr, decode addr $ fromIntegral val) (csrs comp) })
  getPC = MState $ \comp -> return (pc comp, comp)
  setPC val = MState $ \comp -> return ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> return ((), comp { pc = nextPC comp })
