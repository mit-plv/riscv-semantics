{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Utility where
import Data.Bits
import Data.Int
import Data.Word

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (shiftR x start) .&. (complement $ shiftL (-1) (end - start))

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

lower :: (Bits a, Integral a, Num b) => Int -> a -> b
lower n x = fromIntegral $ bitSlice x 0 n

class (Integral s, Integral u) => Convertible s u | s -> u, u -> s where
  unsigned :: s -> u
  unsigned = fromIntegral
  signed :: u -> s
  signed = fromIntegral

instance Convertible Int8 Word8
instance Convertible Int16 Word16
instance Convertible Int32 Word32
instance Convertible Int64 Word64

class MachineWidth t where
  shiftBits :: t -> Int

instance MachineWidth Int32 where
  shiftBits = lower 5

instance MachineWidth Int64 where
  shiftBits = lower 6
