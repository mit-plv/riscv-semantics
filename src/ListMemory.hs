{-# LANGUAGE FlexibleInstances #-}
module ListMemory where
import Memory
import Utility
import Data.Word

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

instance Memory [Word8] where
  loadByte mem addr = mem !! (fromIntegral addr)
  loadHalf mem addr = combineBytes $ take 2 $ drop (fromIntegral addr) mem
  loadWord mem addr = combineBytes $ take 4 $ drop (fromIntegral addr) mem
  loadDouble mem addr = combineBytes $ take 8 $ drop (fromIntegral addr) mem
  storeByte mem addr val = setIndex (fromIntegral addr) val mem
  storeHalf mem addr val = helpStore mem (fromIntegral addr) (splitHalf val)
  storeWord mem addr val = helpStore mem (fromIntegral addr) (splitWord val)
  storeDouble mem addr val = helpStore mem (fromIntegral addr) (splitDouble val)
