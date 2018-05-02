{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs #-}
module Minimal32 where
import Program
import Decode
import Utility
import CSRFile
import qualified CSRField as Field
import qualified Memory as M
import MapMemory()
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Map as S
import Control.Monad.State

data Minimal32 = Minimal32 { registers :: [Int32], csrs :: CSRFile, pc :: Int32,
                             nextPC :: Int32, privMode :: PrivMode, mem :: S.Map Int (Tag,Word32) }
               deriving (Show)

type MState = State Minimal32

type LoadFunc = MState (Tag,Int32)
type StoreFunc = (Tag,Int32) -> MState ()

instance (Show LoadFunc) where
  show _ = "<loadfunc>"
instance (Show StoreFunc) where
  show _ = "<storefunc>"


data Tag = Bare | Overflowed deriving (Show,Eq)
data TaggedInt = TInt { tag :: Tag, int :: Int32} deriving (Show, Eq, Ord, Num, Integral)
pollute Overflowed _ = Overflowed
pollute _ Overflowed = Overflowed
pollute Bare Bare = Bare

--
--instance Real (Tag,Int32) where
--
--instance Enum (Tag,Int32) where
--instance Num (Tag,Int32) where
--

instance Integral (Tag,Int32) where
   toInteger a = toInteger $ snd a
   quotRem a b = (\(x,y) -> ((tag,x),(tag,y))) $ quotRem (snd a) (snd b)
       where tag = pollute (fst a) (fst b)
-- write toInteger and fromInteger

instance RiscvProgram MState (Tag,Int32) where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! ((fromIntegral:: Register -> Int) reg-1), comp)
  setRegister :: forall s. (Integral s) => Register -> s -> MState ()
  setRegister reg val = state $ \comp -> if (fst val == Overflowed) 
                                          then err "Overflowed" 
                                          else ((), if reg == 0 then comp else comp { registers = setIndex ((fromIntegral:: Register -> Int) reg-1) (fromIntegral val) (registers comp) })
  getPC = state $ \comp -> (pc comp, comp)
  setPC :: forall s. (Integral s) => s -> MState ()
  setPC val = state $ \comp -> if (fst val == Overflowed) 
                                          then err "Overflowed" 
                                          else ((), comp { nextPC = fromIntegral val })
  getPrivMode = state $ \comp -> (privMode comp, comp)
  setPrivMode val = state $ \comp -> ((), comp { privMode = val })
  commit = do
    state $ \comp -> ((), comp { pc = fPC })
  -- Wrap Memory instance:
  loadWord :: forall s. (Integral s) => s -> MState (Tag,Int32)
  loadWord addr =
      state $ \comp -> ((mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)), comp)
  storeWord :: forall s. (Integral s, Bits s) => s -> (Tag,Int32) -> MState ()
  storeWord addr val =
      state $ \comp -> if (fst val == Overflowed) 
                                          then err "Overflowed" 
                                          else ((), comp { mem = storeFunc (mem comp) ((fromIntegral:: Word32 -> Int) ((fromIntegral:: a' -> Word32) addr)) ((fromIntegral:: v' -> v) val) })
  -- CSRs:
  getCSRField field = state $ \comp -> (getField field (csrs comp), comp)
  setCSRField :: forall s. (Integral s) => Field.CSRField -> s -> MState ()
  setCSRField field val = state $ \comp -> ((), comp { csrs = setField field ((fromIntegral:: s -> MachineInt) val) (csrs comp) })
  -- Unimplemented:
