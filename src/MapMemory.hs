{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module MapMemory where
import Memory
import Utility
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Map as S

readM :: forall a. (Integral a) => (S.Map Int Word8) -> a -> Word8
readM mem addr = fromMaybe 0 (S.lookup ((fromIntegral:: a -> Int) addr) mem)

writeM :: forall a. (Integral a) => (S.Map Int Word8) -> a -> Word8 -> (S.Map Int Word8)
writeM mem addr val = S.insert ((fromIntegral:: a -> Int) addr) val mem

helpLoad :: forall a b. (Integral a, Bits b, Integral b) => (S.Map Int Word8) -> a -> Int -> b
helpLoad mem addr numBytes =
  combineBytes $ fmap (\a -> readM mem a) [((fromIntegral:: a -> Int) addr)..((fromIntegral:: a -> Int) addr + numBytes - 1)]

helpStore :: forall a. (Integral a) => (S.Map Int Word8) -> a -> [Word8] -> (S.Map Int Word8)
helpStore mem addr bytes =
  foldr (\(b,a) m -> writeM m a b) mem $ zip bytes [addr + i | i <- [0..]]

instance Memory (S.Map Int Word8) where
  loadByte mem addr = readM mem addr
  loadHalf mem addr = helpLoad mem addr 2
  loadWord mem addr = helpLoad mem addr 4
  loadDouble mem addr = helpLoad mem addr 8
  storeByte mem addr val = writeM mem addr val
  storeHalf mem addr val = helpStore mem addr (splitHalf val)
  storeWord mem addr val = helpStore mem addr (splitWord val)
  storeDouble mem addr val = helpStore mem addr (splitDouble val)
