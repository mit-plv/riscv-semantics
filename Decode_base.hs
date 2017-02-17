module Decode where
import Data.Int
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List

type Register = Int

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (.&.) (shiftR x start) (complement $ shiftL (-1) (end - start))

getRd inst = bitSlice inst 7 12
getRs1 inst = bitSlice inst 15 20
getRs2 inst = bitSlice inst 20 25
getPred inst = bitSlice inst 24 28
getSucc inst = bitSlice inst 20 24
getImm20 inst = shift (bitSlice inst 12 32) 12
getOimm20 inst = shift (bitSlice inst 12 32) 12
getJimm20 inst = (.|.) (shift (bitSlice inst 31 32) 20) $ (.|.) (shift (bitSlice inst 21 31) 1) $
                 (.|.) (shift (bitSlice inst 20 21) 11) (shift (bitSlice inst 12 20) 12)
getImm12 inst = bitSlice inst 20 32
getOimm12 inst = bitSlice inst 20 32
getCsr12 inst = bitSlice inst 20 32
getSimm12 inst = (.|.) (shift (bitSlice inst 25 32) 5) (bitSlice inst 7 12)
getSbimm12 inst = (.|.) (shift (bitSlice inst 31 32) 12) $ (.|.) (shift (bitSlice inst 25 31) 5) $
                  (.|.) (shift (bitSlice inst 8 12) 1) (shift (bitSlice inst 7 8) 11)
getShamt5 inst = bitSlice inst 20 25

decode :: Int -> Instruction
decode inst = (fst $ fromJust $ find (\e -> all match (snd e)) opcodeTable) inst
              where match (start, end, val) = bitSlice inst start end == val

-- Auto-generated code begins here. --
