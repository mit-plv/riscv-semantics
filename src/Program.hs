{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Program where
import CSRField
import Decode
import Utility
import Data.Int
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Prelude

class (Monad p, Convertible t u, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => RiscvProgram p t u | p -> t, t -> u where
  getRegister :: Register -> p t
  setRegister :: (Integral s) => Register -> s -> p ()
  loadByte :: (Integral s) => s -> p Int8
  loadHalf :: (Integral s) => s -> p Int16
  loadWord :: (Integral s) => s -> p Int32
  loadDouble :: (Integral s) => s -> p Int64
  storeByte :: (Integral s, Bits s) => s -> Int8 -> p ()
  storeHalf :: (Integral s, Bits s) => s -> Int16 -> p ()
  storeWord :: (Integral s, Bits s) => s -> Int32 -> p ()
  storeDouble :: (Integral s, Bits s) => s -> Int64 -> p ()
  getCSRField :: CSRField -> p MachineInt
  setCSRField :: (Integral s) => CSRField -> s -> p ()
  getPC :: p t
  setPC :: (Integral s) => s -> p ()
  step :: p ()

getXLEN :: forall p t u s. (RiscvProgram p t u, Integral s) => p s
getXLEN = do
            mxl <- getCSRField MXL
            case mxl of
                1 -> return 32
                2 -> return 64

instance (RiscvProgram p t u) => RiscvProgram (MaybeT p) t u where
  getRegister r = lift (getRegister r)
  setRegister r v = lift (setRegister r v)
  loadByte a = lift (loadByte a)
  loadHalf a = lift (loadHalf a)
  loadWord a = lift (loadWord a)
  loadDouble a = lift (loadDouble a)
  storeByte a v = lift (storeByte a v)
  storeHalf a v = lift (storeHalf a v)
  storeWord a v = lift (storeWord a v)
  storeDouble a v = lift (storeDouble a v)
  getCSRField f = lift (getCSRField f)
  setCSRField f v = lift (setCSRField f v)
  getPC = lift getPC
  setPC v = lift (setPC v)
  step = lift step

raiseException :: (RiscvProgram p t u) => MachineInt -> MachineInt -> p ()
raiseException isInterrupt exceptionCode = do
  pc <- getPC
  addr <- getCSRField MTVecBase
  setCSRField MEPC pc
  setCSRField MCauseInterrupt isInterrupt
  setCSRField MCauseCode exceptionCode
  setPC (addr * 4)

slli :: forall t u .(Convertible t u, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => t -> MachineInt -> t
slli x shamt6 = (shiftL x (shiftBits ((fromIntegral:: MachineInt -> t) shamt6)))

srli :: forall t u . (Convertible t u, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => t -> MachineInt -> u
srli x shamt6 = (shiftR ((unsigned x) :: u) (shiftBits ((fromIntegral:: MachineInt -> t) shamt6)))

srai :: forall t u . (Convertible t u, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => t -> MachineInt -> t
srai x shamt6 = (shiftR x (shiftBits ((fromIntegral:: MachineInt -> t) shamt6)))

sll x y = (shiftL x (shiftBits y))
srl x y = (shiftR (unsigned x) (shiftBits y))
sra x y =(shiftR x (shiftBits y))


ltu :: forall t u s . (Convertible t u, Integral s, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => t -> s -> Bool
ltu x y = (unsigned  x) < ((fromIntegral:: s -> u) y)

