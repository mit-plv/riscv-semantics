{-# LANGUAGE FlexibleInstances #-}
module MapMemory where
import Memory
import Utility
import Data.Maybe
import Data.Word
import qualified Data.Map as S

readM mem addr = fromMaybe 0 (S.lookup (fromIntegral addr) mem)
writeM mem addr val = S.insert (fromIntegral addr) val mem

helpLoad mem addr numBytes =
  combineBytes $ fmap (\addr -> readM mem addr) [(fromIntegral addr)..(fromIntegral addr + numBytes - 1)]
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
