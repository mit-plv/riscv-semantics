{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}
module Utility where
import Data.Bits
import Data.Int
import Data.Word
import Prelude
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
type MachineInt = Int64


bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (shiftR x start) .&. (complement $ shiftL (-1) (end - start))
{-# SPECIALIZE bitSlice :: Word32 -> Int -> Int -> Word32 #-}
{-# INLINE bitSlice #-}
setIndex :: Int -> a -> [a] -> [a]
setIndex i x l = left ++ (x:(drop 1 right))
  where (left, right) = splitAt i l

s8 :: forall t. (Integral t) => t -> t
s8 n = (fromIntegral:: Int8 -> t) ((fromIntegral:: t -> Int8) n)
{-# INLINE s8 #-}

s16 :: forall t. (Integral t) => t -> t
s16 n = (fromIntegral:: Int16 -> t) ((fromIntegral:: t -> Int16) n)
{-# INLINE s16 #-}

s32 :: forall t. (Integral t) => t -> t
s32 n = (fromIntegral:: Int32 -> t) ((fromIntegral:: t -> Int32) n)
{-# INLINE s32 #-}

u8 :: forall t. (Integral t) => t -> t
u8 n = (fromIntegral:: Word8 -> t) ((fromIntegral:: t -> Word8) n)
{-# INLINE u8 #-}

u16 :: forall t. (Integral t) => t -> t
u16 n = (fromIntegral:: Word16 -> t) ((fromIntegral:: t -> Word16) n)
{-# INLINE u16 #-}

u32 :: forall t. (Integral t) => t -> t
u32 n = (fromIntegral:: Word32 -> t) ((fromIntegral:: t -> Word32) n)
{-# INLINE u32 #-}

lower :: forall a b. (Bits a, Integral a, Num b) => Int -> a -> b
lower n x = (fromIntegral:: a -> b) $ bitSlice x 0 n
{-# INLINE lower #-}

combineBytes :: forall a. (Bits a, Integral a) => [Word8] -> a
combineBytes bytes = foldr (\(x,n) res -> res .|. shiftL ((fromIntegral:: Word8 -> a) n) (8*x)) 0 $ zip [0..] bytes
{-# INLINE combineBytes #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word64 #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word32 #-}
{-# SPECIALIZE combineBytes :: [Word8] -> Word16 #-}

splitBytes :: forall a. (Bits a, Integral a) => Int -> a -> [Word8]
splitBytes n w = map (fromIntegral:: a -> Word8)  [bitSlice w p (p + 8) | p <- [0,8..n-1]]

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


-- signed values in a register (we always use signed by default)
class (Integral t, Bits t) => MachineWidth t where
  fromImm :: MachineInt -> t
  fromImm = fromIntegral

  regToInt8 :: t -> Int8
  regToInt8 = fromIntegral

  regToInt16 :: t -> Int16
  regToInt16 = fromIntegral

  regToInt32 :: t -> Int32
  regToInt32 = fromIntegral

  regToInt64 :: t -> Int64
  regToInt64 = fromIntegral

  uInt8ToReg :: forall u. (Convertible t u) => Int8 -> t
  uInt8ToReg x = fromIntegral (unsigned x)

  uInt16ToReg :: forall u. (Convertible t u) => Int16 -> t
  uInt16ToReg x = fromIntegral (unsigned x)

  uInt32ToReg :: forall u. (Convertible t u) => Int32 -> t
  uInt32ToReg x = fromIntegral (unsigned x)

  uInt64ToReg :: forall u. (Convertible t u) => Int64 -> t
  uInt64ToReg x = fromIntegral (unsigned x)

  int8ToReg :: Int8 -> t
  int8ToReg = fromIntegral

  int16ToReg :: Int16 -> t
  int16ToReg = fromIntegral

  int32ToReg :: Int32 -> t
  int32ToReg = fromIntegral

  int64ToReg :: Int64 -> t
  int64ToReg = fromIntegral

  regToZ_signed :: t -> Integer
  regToZ_signed = fromIntegral

  regToZ_unsigned :: forall u. (Convertible t u) => t -> Integer
  regToZ_unsigned = (fromIntegral :: u -> Integer) . (unsigned :: t -> u)
  
  sll :: t -> Int -> t
  sll = shiftL

  srl :: forall u. (Convertible t u, Bits u) => t -> Int -> t
  srl x y = signed (shiftR (unsigned x) y)

  sra :: t -> Int -> t
  sra = shiftR

  regToShamt5 :: t -> Int

  regToShamt :: t -> Int

  highBits :: Integer -> t


instance MachineWidth Int32 where
  regToShamt5 = lower 5
  regToShamt = lower 5
  highBits n = (fromIntegral:: Integer -> Int32) $ bitSlice n 32 64

instance MachineWidth Int64 where
  regToShamt5 = lower 5
  regToShamt = lower 6
  highBits n = (fromIntegral:: Integer -> Int64) $ bitSlice n 64 128


liftState :: (Monad m) => State a b -> StateT a m b
liftState = mapStateT (return . runIdentity)
