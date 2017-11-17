{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Program where
import CSRField
import Decode
import Utility
import Data.Int
import Data.Bits
import Control.Monad
import Prelude

class (MonadPlus p, Convertible t u, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => RiscvProgram p t u | p -> t, t -> u where
  getXLEN :: (Integral s) => p s
  getRegister :: Register -> p t
  setRegister :: (Integral s) => Register -> s -> p ()
  loadByte :: (Integral s) => s -> p Int8
  loadHalf :: (Integral s) => s -> p Int16
  loadWord :: (Integral s) => s -> p Int32
  loadDouble :: (Integral s) => s -> p Int64
  storeByte :: (Integral r, Integral s, Bits r, Bits s) => r -> s -> p ()
  storeHalf :: (Integral r, Integral s, Bits r, Bits s) => r -> s -> p ()
  storeWord :: (Integral r, Integral s, Bits r, Bits s) => r -> s -> p ()
  storeDouble :: (Integral r, Integral s, Bits r, Bits s) => r -> s -> p ()
  getCSRField :: CSRField -> p MachineInt
  setCSRField :: (Integral s) => CSRField -> s -> p ()
  getPC :: p t
  setPC :: (Integral s) => s -> p ()
  step :: p ()

raiseException :: (RiscvProgram p t u) => MachineInt -> MachineInt -> p ()
raiseException isInterrupt exceptionCode = do
  pc <- getPC
  addr <- getCSRField MTVecBase
  setCSRField MEPC pc
  setCSRField MCauseInterrupt isInterrupt
  setCSRField MCauseCode exceptionCode
  setPC (addr * 4)
