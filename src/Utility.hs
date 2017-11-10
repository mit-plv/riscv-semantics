{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Utility where
import Data.Bits
import Data.Int
import Data.Word
import Prelude

type MachineInt = Int64


bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (shiftR x start) .&. (complement $ shiftL (-1) (end - start))
{-# SPECIALIZE bitSlice :: Word32 -> Int -> Int -> Word32 #-}
{-# INLINE bitSlice #-}
setIndex :: Int -> a -> [a] -> [a]
setIndex i x l = left ++ (x:(drop 1 right))
  where (left, right) = splitAt i l

s8 :: (Integral t) => t -> t
s8 n = fromIntegral (fromIntegral n :: Int8)
{-# INLINE s8 #-}

s16 :: (Integral t) => t -> t
s16 n = fromIntegral (fromIntegral n :: Int16)
{-# INLINE s16 #-}

s32 :: (Integral t) => t -> t
s32 n = fromIntegral (fromIntegral n :: Int32)
{-# INLINE s32 #-}

u8 :: (Integral t) => t -> t
u8 n = fromIntegral (fromIntegral n :: Word8)
{-# INLINE u8 #-}

u16 :: (Integral t) => t -> t
u16 n = fromIntegral (fromIntegral n :: Word16)
{-# INLINE u16 #-}

u32 :: (Integral t) => t -> t
u32 n = fromIntegral (fromIntegral n :: Word32)
{-# INLINE u32 #-}

lower :: (Bits a, Integral a, Num b) => Int -> a -> b
lower n x = fromIntegral $ bitSlice x 0 n
{-# INLINE lower #-}

combineBytes :: (Bits a, Integral a) => [Word8] -> a
combineBytes bytes = foldr (\(x,n) res -> res .|. shiftL (fromIntegral n) (8*x)) 0 $ zip [0..] bytes
{-# INLINE combineBytes #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word64 #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word32 #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word16 #-}

splitBytes :: (Bits a, Integral a) => Int -> a -> [Word8]
splitBytes n w = map fromIntegral [bitSlice w p (p + 8) | p <- [0,8..n-1]]

splitHalf :: (Bits a, Integral a) => a -> [Word8]
splitHalf = splitBytes 16
{-# INLINE splitHalf #-}

splitWord :: (Bits a, Integral a) => a -> [Word8]
splitWord = splitBytes 32
{-# INLINE splitWord #-}

splitDouble :: (Bits a, Integral a) => a -> [Word8]
splitDouble = splitBytes 64
{-# INLINE splitDouble #-}

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
  highBits :: Int -> t

instance MachineWidth Int32 where
  shiftBits = lower 5
  highBits n = fromIntegral $ bitSlice n 32 64

instance MachineWidth Int64 where
  shiftBits = lower 6
  highBits n = fromIntegral $ bitSlice n 64 128
