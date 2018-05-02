{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MapMemory where
import Memory
import Utility
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Map as S
import Clash.Prelude
import Data.List as L
readM :: (S.Map Int Word8) -> Int -> Word8
readM mem addr = fromMaybe 0 (S.lookup addr mem)

writeM :: (S.Map Int Word8) -> Int -> Word8 -> (S.Map Int Word8)
writeM mem addr val = S.insert addr val mem

helpLoad :: (Bits b, Integral b) => (S.Map Int Word8) -> Int -> Int -> b
helpLoad mem addr numBytes =
  combineBytes $ fmap (\a -> readM mem a) [addr..(addr + numBytes - 1)]

helpStore :: (S.Map Int Word8) -> Int -> [Word8] -> (S.Map Int Word8)
helpStore mem addr bytes =
  L.foldr (\(b,a) m -> writeM m a b) mem $ L.zip bytes [addr + i | i <- [0..]]

instance Memory (S.Map Int Word8) Int where
  loadByte mem addr = readM mem addr
  loadHalf mem addr = helpLoad mem addr 2
  loadWord mem addr = helpLoad mem addr 4
  loadDouble mem addr = helpLoad mem addr 8
  storeByte mem addr val = writeM mem addr val
  storeHalf mem addr val = helpStore mem addr (splitHalf val)
  storeWord mem addr val = helpStore mem addr (splitWord val)
  storeDouble mem addr val = helpStore mem addr (splitDouble val)
