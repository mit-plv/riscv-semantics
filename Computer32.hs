{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Computer32 where
import Program
import Utility
import Data.Int
import Data.Word
import Data.Bits
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

data Computer32 = Computer32 { registers :: [Int32], pc :: Int32, mem :: [Word8] }
                deriving (Show)

combineBytes :: (Bits a, Integral a) => [Word8] -> a
combineBytes bytes = sum $ map (\(x,n) -> shiftL (fromIntegral n) (8*x)) $ zip [0..] bytes

splitHalf :: (Bits a, Integral a) => a -> [Word8]
splitHalf w = map fromIntegral [bitSlice w 0 8, bitSlice w 8 16]

splitWord :: (Bits a, Integral a) => a -> [Word8]
splitWord w = map fromIntegral [bitSlice w 0 8, bitSlice w 8 16, bitSlice w 16 24, bitSlice w 24 32]

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

instance RiscvProgram (MState Computer32) Int32 Word32 where
  getRegister reg = MState $ \comp -> Just ((registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> Just ((), comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte addr = MState $ \comp -> Just (fromIntegral $ (mem comp) !! (fromIntegral addr), comp)
  loadHalf addr = MState $ \comp -> Just (combineBytes $ take 2 $ drop (fromIntegral addr) (mem comp), comp)
  loadWord addr = MState $ \comp -> Just (combineBytes $ take 4 $ drop (fromIntegral addr) (mem comp), comp)
  storeByte addr val = MState $ \comp -> Just ((), comp { mem = setIndex (fromIntegral addr) (fromIntegral val) (mem comp) })
  storeHalf addr val = MState $ \comp -> Just ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitHalf val) })
  storeWord addr val = MState $ \comp -> Just ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitWord val) })
  getPC = MState $ \comp -> Just (pc comp, comp)
  setPC val = MState $ \comp -> Just ((), comp { pc = fromIntegral val })
