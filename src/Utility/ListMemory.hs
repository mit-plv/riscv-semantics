{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
module Utility.ListMemory where
import Spec.Memory
import Utility.Utility
import Data.Word

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

instance Memory [Word8] Int where
  loadByte :: [Word8] -> Int -> Word8
  loadByte mem addr = mem !! addr

  loadHalf :: [Word8] -> Int -> Word16
  loadHalf mem addr = combineBytes $ take 2 $ drop addr mem

  loadWord :: [Word8] -> Int -> Word32
  loadWord mem addr = combineBytes $ take 4 $ drop addr mem

  loadDouble :: [Word8] -> Int -> Word64
  loadDouble mem addr = combineBytes $ take 8 $ drop addr mem

  storeByte :: [Word8] -> Int -> Word8 -> [Word8]
  storeByte mem addr val = setIndex addr val mem

  storeHalf :: [Word8] -> Int -> Word16 -> [Word8]
  storeHalf mem addr val = helpStore mem addr (splitHalf val)

  storeWord :: [Word8] -> Int -> Word32 -> [Word8]
  storeWord mem addr val = helpStore mem addr (splitWord val)

  storeDouble :: [Word8] -> Int -> Word64 -> [Word8]
  storeDouble mem addr val = helpStore mem addr (splitDouble val)
