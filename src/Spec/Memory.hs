{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.Memory where
import Data.Word

class Memory m a where
  loadByte :: m -> a -> Word8
  loadHalf :: m -> a -> Word16
  loadWord :: m -> a -> Word32
  loadDouble :: m -> a -> Word64
  storeByte :: m -> a -> Word8 -> m
  storeHalf :: m -> a -> Word16 -> m
  storeWord :: m -> a -> Word32 -> m
  storeDouble :: m -> a -> Word64 -> m
  makeReservation :: m -> a -> m
  checkReservation :: m -> a -> Bool
  clearReservation :: m -> a -> m
