{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteCSR where
import Spec.CSR
import qualified Spec.CSRField as Field
import Spec.CSRSpec
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Data.Bits
import Control.Monad
import Prelude

checkPermissions isWrite csr = do
  let readOnly = bitSlice csr 10 12 == 3
  let minMode = decodePrivMode (bitSlice csr 8 10)
  mode <- getPrivMode
  when ((mode < minMode) || (readOnly && isWrite)) (raiseException 0 2)

execute :: forall p t. (RiscvMachine p t) => InstructionCSR -> p ()
-- begin ast
execute (Csrrw rd rs1 csr12) = do
  checkPermissions True csr12
  x <- getRegister rs1
  when (rd /= 0) (do
    y <- getCSR (lookupCSR csr12)
    setRegister rd ((fromIntegral:: MachineInt -> t) y))
  setCSR (lookupCSR csr12) x
execute (Csrrs rd rs1 csr12) = do
  checkPermissions (rs1 /= 0) csr12
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .|. ((fromIntegral:: t -> MachineInt) mask)))
execute (Csrrc rd rs1 csr12) = do
  checkPermissions (rs1 /= 0) csr12
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .&. (complement ((fromIntegral:: t -> MachineInt) mask))))
execute (Csrrwi rd zimm csr12) = do
  checkPermissions True csr12
  when (rd /= 0) (do
    val <- getCSR (lookupCSR csr12)
    setRegister rd ((fromIntegral:: MachineInt -> t) val))
  setCSR (lookupCSR csr12) zimm
execute (Csrrsi rd zimm csr12) = do
  checkPermissions (zimm /= 0) csr12
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (zimm /= 0) (setCSR (lookupCSR csr12) (val .|. zimm))
execute (Csrrci rd zimm csr12) = do
  checkPermissions (zimm /= 0) csr12
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (zimm /= 0) (setCSR (lookupCSR csr12) (val .&. (complement zimm)))
execute Ecall = do
  mode <- getPrivMode
  case mode of
        User -> raiseException 0 8
        Supervisor -> raiseException 0 9
        Machine -> raiseException 0 11
execute Ebreak = do
  raiseException 0 3
execute Mret = do
  priv <- getPrivMode
  when (priv < Machine) (raiseException 0 2)
  mpp <- getCSRField Field.MPP
  setCSRField Field.MPP (encodePrivMode User)
  setPrivMode (decodePrivMode mpp)
  mpie <- getCSRField Field.MPIE
  setCSRField Field.MPIE 1
  setCSRField Field.MIE mpie
  mepc <- getCSRField Field.MEPC
  setPC ((fromIntegral:: MachineInt -> t) mepc)
execute Sret = do
  priv <- getPrivMode
  when (priv < Supervisor) (raiseException 0 2)
  tsr <- getCSRField Field.TSR
  when (tsr == 1) (raiseException 0 2)
  spp <- getCSRField Field.SPP
  setCSRField Field.SPP (encodePrivMode User)
  setPrivMode (decodePrivMode spp)
  spie <- getCSRField Field.SPIE
  setCSRField Field.SPIE 1
  setCSRField Field.SIE spie
  sepc <- getCSRField Field.SEPC
  setPC ((fromIntegral:: MachineInt -> t) sepc)
execute Wfi = do
  mode <- getPrivMode
  tw <- getCSRField Field.TW
  when (mode == Supervisor && tw == 1) (raiseException 0 2)
execute (Sfence_vma vaddr asid) = do
  priv <- getPrivMode
  tvm <- getCSRField Field.TVM
  when (priv == Supervisor && tvm == 1) (raiseException 0 2)
  flushTLB
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst