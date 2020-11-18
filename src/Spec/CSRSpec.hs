{-# LANGUAGE ScopedTypeVariables #-}
module Spec.CSRSpec where
import qualified Spec.CSRGetSet as GetSet
import Spec.CSR
import Spec.Machine
import Utility.Utility
import Control.Monad
import Prelude

getCSR :: forall p t . (RiscvMachine p t) => CSR -> p t
getCSR c = do
  r <- GetSet.getCSR c
  return ((fromIntegral:: MachineInt -> t) r)

setCSR :: forall p t . (RiscvMachine p t) => CSR -> t -> p ()
setCSR c v = GetSet.setCSR c ((fromIntegral:: t -> MachineInt) v)
