{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MapMemory where
import Memory
import Utility
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.Map.Strict as S
import Prelude
import Data.List as L
readM :: (S.Map Int Word8) -> Int -> Word8
readM mem addr = fromMaybe 0 (S.lookup addr mem)

writeM :: (S.Map Int Word8) -> Int -> Word8 -> (S.Map Int Word8)
writeM mem addr 0 = S.delete addr mem
writeM mem addr val = S.insert addr val mem

helpLoad :: (Bits b, Integral b) => (S.Map Int Word8) -> Int -> Int -> b
helpLoad mem addr numBytes =
  combineBytes $ fmap (\a -> readM mem a) [addr..(addr + numBytes - 1)]

helpStore :: (S.Map Int Word8) -> Int -> [Word8] -> (S.Map Int Word8)
helpStore mem addr bytes =
  L.foldr (\(b,a) m -> writeM m a b) mem $ L.zip bytes [addr + i | i <- [0..]]

data MapMemory a = MapMemory { bytes :: S.Map a Word8, reservation :: Maybe a }
  deriving (Eq, Show)

instance Memory (MapMemory Int) Int where
  loadByte mem addr = readM (bytes mem) addr
  loadHalf mem addr = helpLoad (bytes mem) addr 2
  loadWord mem addr = helpLoad (bytes mem) addr 4
  loadDouble mem addr = helpLoad (bytes mem) addr 8
  storeByte mem addr val = mem { bytes = writeM (bytes mem) addr val }
  storeHalf mem addr val = mem { bytes = helpStore (bytes mem) addr (splitHalf val) }
  storeWord mem addr val = mem { bytes = helpStore (bytes mem) addr (splitWord val) }
  storeDouble mem addr val = mem { bytes = helpStore (bytes mem) addr (splitDouble val) }
  makeReservation mem addr = mem { reservation = Just addr }
  checkReservation mem addr = (reservation mem) == Just addr
  clearReservation mem addr = mem { reservation = Nothing }
