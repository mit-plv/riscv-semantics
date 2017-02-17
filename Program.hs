{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables #-}
module Program where
import Decode
import Data.Bits
import Data.Int
import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

-- Utility function.
setIndex :: Int -> a -> [a] -> [a]
setIndex i x l = left ++ (x:(drop 1 right))
  where (left, right) = splitAt i l

s8 :: (Integral t) => t -> t
s8 n = fromIntegral (fromIntegral n :: Int8)

s16 :: (Integral t) => t -> t
s16 n = fromIntegral (fromIntegral n :: Int16)

s32 :: (Integral t) => t -> t
s32 n = fromIntegral (fromIntegral n :: Int32)

u8 :: (Integral t) => t -> t
u8 n = fromIntegral (fromIntegral n :: Word8)

u16 :: (Integral t) => t -> t
u16 n = fromIntegral (fromIntegral n :: Word16)

u32 :: (Integral t) => t -> t
u32 n = fromIntegral (fromIntegral n :: Word32)

lower5 :: (Bits a, Integral a, Num b) => a -> b
lower5 x = fromIntegral $ bitSlice x 0 5

class (Monad p, Integral t, Bits t, Integral u, Bits u) => RiscvProgram p t u | p -> t, p -> u where
   getRegister :: Register -> p t
   setRegister :: (Integral s) => Register -> s -> p ()
   load :: (Integral s) => s -> p t
   store :: (Integral r, Integral s) => r -> s -> p ()
   getPC :: p t
   setPC :: (Integral s) => s -> p ()

data Computer32 = Computer32 { registers :: [Int32], pc :: Int32, mem :: [Int32] }
                deriving (Show)

instance RiscvProgram (State Computer32) Int32 Word32 where
  getRegister reg = state $ \comp -> ((registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp -> ((), comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  load addr = state $ \comp -> ((mem comp) !! (fromIntegral addr), comp)
  store addr val = state $ \comp -> ((), comp { mem = setIndex (fromIntegral addr) (fromIntegral val) (mem comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC val = state $ \comp -> ((), comp { pc = fromIntegral val })
