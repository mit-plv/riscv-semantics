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
  setRegister :: Register -> t -> p ()
  loadByte :: t -> p Int8
  loadHalf :: t -> p Int16
  loadWord :: t -> p Int32
  loadDouble :: t -> p Int64
  storeByte :: t -> Int8 -> p ()
  storeHalf :: t -> Int16 -> p ()
  storeWord :: t -> Int32 -> p ()
  storeDouble :: t -> Int64 -> p ()
  getCSRField :: CSRField -> p MachineInt
  setCSRField :: (Integral s) => CSRField -> s -> p ()
  getPC :: p t
  setPC :: t -> p ()
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

raiseException :: forall p t u. (RiscvProgram p t u) => MachineInt -> MachineInt -> p ()
raiseException isInterrupt exceptionCode = do
  pc <- getPC
  addr <- getCSRField MTVecBase
  setCSRField MEPC pc
  setCSRField MCauseInterrupt isInterrupt
  setCSRField MCauseCode exceptionCode
  setPC ((fromIntegral:: MachineInt -> t) addr * 4)


ltu :: forall t u s . (Convertible t u, Integral s, Bounded t, Bounded u, Bits t, Bits u, MachineWidth t) => t -> s -> Bool
ltu x y = (unsigned  x) < ((fromIntegral:: s -> u) y)

