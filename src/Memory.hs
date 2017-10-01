module Memory where
import Data.Word

class Memory m where
  loadByte :: (Integral a) => m -> a -> Word8
  loadHalf :: (Integral a) => m -> a -> Word16
  loadWord :: (Integral a) => m -> a -> Word32
  loadDouble :: (Integral a) => m -> a -> Word64
  storeByte :: (Integral a) => m -> a -> Word8 -> m
  storeHalf :: (Integral a) => m -> a -> Word16 -> m
  storeWord :: (Integral a) => m -> a -> Word32 -> m
  storeDouble :: (Integral a) => m -> a -> Word64 -> m
