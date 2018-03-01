{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, InstanceSigs #-}
module ListMemory where
import Memory
import Utility
import Data.Word

helpStore mem addr bytes = f ++ bytes ++ drop (length bytes) s
  where (f,s) = splitAt addr mem

instance Memory [Word8] where
  loadByte :: forall a. (Integral a) => [Word8] -> a -> Word8
  loadByte mem addr = mem !! ((fromIntegral:: a -> Int) addr)

  loadHalf :: forall a. (Integral a) => [Word8] -> a -> Word16
  loadHalf mem addr = combineBytes $ take 2 $ drop ((fromIntegral:: a -> Int) addr) mem

  loadWord :: forall a. (Integral a) => [Word8] -> a -> Word32
  loadWord mem addr = combineBytes $ take 4 $ drop ((fromIntegral:: a -> Int) addr) mem

  loadDouble :: forall a. (Integral a) => [Word8] -> a -> Word64
  loadDouble mem addr = combineBytes $ take 8 $ drop ((fromIntegral:: a -> Int) addr) mem

  storeByte :: forall a. (Integral a) => [Word8] -> a -> Word8 -> [Word8]
  storeByte mem addr val = setIndex ((fromIntegral:: a -> Int) addr) val mem

  storeHalf :: forall a. (Integral a) => [Word8] -> a -> Word16 -> [Word8]
  storeHalf mem addr val = helpStore mem ((fromIntegral:: a -> Int) addr) (splitHalf val)

  storeWord :: forall a. (Integral a) => [Word8] -> a -> Word32 -> [Word8]
  storeWord mem addr val = helpStore mem ((fromIntegral:: a -> Int) addr) (splitWord val)

  storeDouble :: forall a. (Integral a) => [Word8] -> a -> Word64 -> [Word8]
  storeDouble mem addr val = helpStore mem ((fromIntegral:: a -> Int) addr) (splitDouble val)
