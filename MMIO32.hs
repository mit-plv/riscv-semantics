{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module MMIO32 where
import Prelude hiding (getChar, putChar)
import Program
import Utility
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative
import Control.Monad

newtype MState s a = MState { runState :: s -> Maybe (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> fmap (\(b,s) -> (f b, s)) (runState a state)

instance Applicative (MState s) where
  pure x = MState $ \state -> Just (x, state)
  (<*>) f a = MState $ \state -> do
    (g, s1) <- runState f state
    (b, s2) <- runState a s1
    return (g b, s2)

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> runState a state >>= (\(b, s) -> runState (f b) s)

instance Alternative (MState s) where
  empty = MState $ \state -> Nothing
  (<|>) a b = MState $ \state -> runState a state <|> runState b state

instance MonadPlus (MState s)

data MMIO32 = MMIO32 { registers :: [Int32], pc :: Int32, nextPC :: Int32, mem :: [Word8], mmio :: [(MMIO32 -> (Int32, MMIO32), MMIO32 -> Int32 -> MMIO32)], input :: String, output :: String }
                deriving (Show)

instance (Show (MMIO32 -> (Int32, MMIO32))) where
  show x = "<loadfunc>"
instance (Show (MMIO32 -> Int32 -> MMIO32)) where
  show x = "<storefunc>"

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

putCharAddr :: MMIO32 -> Int32
putCharAddr comp = fromIntegral $ (length $ mem comp) + 4

getCharAddr :: MMIO32 -> Int32
getCharAddr comp = fromIntegral $ (length $ mem comp) + 8

ord32 :: Char -> Int32
ord32 = fromIntegral . ord

chr32 :: Int32 -> Char
chr32 = chr . fromIntegral


tryHead "" = -1
tryHead (x:xs) = ord32 x

tryTail "" = ""
tryTail (x:xs) = xs

getChar :: MMIO32 -> (Int32, MMIO32)
getChar comp = (tryHead $ input comp, comp { input = tryTail (input comp) })
putChar :: MMIO32 -> Int32 -> MMIO32
putChar comp val = comp { output = output comp ++ [chr32 val] }

baseMMIO = [(getChar, putChar)]

mmioStart :: (Num a) => MMIO32 -> a
mmioStart = (+1) . fromIntegral . length . mem

instance RiscvProgram (MState MMIO32) Int32 Word32 where
  getRegister reg = MState $ \comp -> Just (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> Just ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte addr = MState $ \comp -> Just (fromIntegral $ (mem comp) !! (fromIntegral addr), comp)
  loadHalf addr = MState $ \comp -> Just (combineBytes $ take 2 $ drop (fromIntegral addr) (mem comp), comp)
  loadWord addr = MState $ \comp -> if
    | addr > (mmioStart comp) -> Just $ (fst ((mmio comp) !! ((fromIntegral $ addr - mmioStart comp) `div` 4))) comp
    | otherwise -> Just (combineBytes $ take 4 $ drop (fromIntegral addr) (mem comp), comp)
  storeByte addr val = MState $ \comp -> Just ((), comp { mem = setIndex (fromIntegral addr) (fromIntegral val) (mem comp) })
  storeHalf addr val = MState $ \comp -> Just ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitHalf val) })
  storeWord addr val = MState $ \comp -> if
    | addr > (mmioStart comp) -> Just ((), (snd ((mmio comp) !! ((fromIntegral $ addr - mmioStart comp) `div` 4))) comp (fromIntegral val))
    | otherwise -> Just ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitWord val) })
  getPC = MState $ \comp -> Just (pc comp, comp)
  setPC val = MState $ \comp -> Just ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> Just ((), comp { pc = nextPC comp })
