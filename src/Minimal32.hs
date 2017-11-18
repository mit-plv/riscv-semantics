{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Minimal32 where
import Program
import Utility
import Memory as M
import ListMemory
import Data.Int
import Data.Word
import Data.Bits
import Control.Applicative
import Control.Monad

newtype MState s a = MState { runState :: s -> (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> let (b, s) = runState a state in (f b, s)

instance Applicative (MState s) where
  pure x = MState $ \state -> (x, state)
  (<*>) f a = MState $ \state ->
    (let (f', s') = runState f state
         (a', s'') = runState a s' in
       (f' a', s''))

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> let (b, s) = runState a state in runState (f b) s

data Minimal32 = Minimal32 { registers :: [Int32], pc :: Int32, nextPC :: Int32, mem :: [Word8] }
               deriving (Show)

wrapLoad loadFunc addr = MState $ \comp -> (fromIntegral $ loadFunc (mem comp) addr, comp)
wrapStore storeFunc addr val = MState $ \comp -> ((), comp { mem = storeFunc (mem comp) addr (fromIntegral val) })

instance RiscvProgram (MState Minimal32) Int32 Word32 where
  getXLEN = return 32
  getRegister reg = MState $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  getPC = MState $ \comp -> (pc comp, comp)
  setPC val = MState $ \comp -> ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> ((), comp { pc = nextPC comp })
  -- Wrap Memory instance:
  loadByte = wrapLoad M.loadByte
  loadHalf = wrapLoad M.loadHalf
  loadWord = wrapLoad M.loadWord
  storeByte = wrapStore M.storeByte
  storeHalf = wrapStore M.storeHalf
  storeWord = wrapStore M.storeWord
  -- Unimplemented:
  getCSRField _ = return 0
  setCSRField _ _ = return ()
  loadDouble _ = return 0
  storeDouble _ _ = return ()
