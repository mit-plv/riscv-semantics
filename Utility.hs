module Utility where
import Data.Bits
import Data.Int
import Data.Word

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (.&.) (shiftR x start) (complement $ shiftL (-1) (end - start))

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
