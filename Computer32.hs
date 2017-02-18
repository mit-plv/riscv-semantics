{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Computer32 where
import Program
import Utility
import Data.Int
import Data.Word
import Control.Monad.State

data Computer32 = Computer32 { registers :: [Int32], pc :: Int32, mem :: [Int32] }
                deriving (Show)

instance RiscvProgram (State Computer32) Int32 Word32 where
  getRegister reg = state $ \comp -> ((registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp -> ((), comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  load addr = state $ \comp -> ((mem comp) !! (fromIntegral addr), comp)
  store addr val = state $ \comp -> ((), comp { mem = setIndex (fromIntegral addr) (fromIntegral val) (mem comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC val = state $ \comp -> ((), comp { pc = fromIntegral val })
