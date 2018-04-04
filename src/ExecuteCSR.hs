{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteCSR where
import CSR
import qualified CSRField as Field
import CSRSpec
import Decode
import Program
import Utility
import Data.Bits
import Control.Monad
import Prelude

execute :: forall p t. (RiscvProgram p t) => InstructionCSR -> p ()
-- begin ast
execute (Csrrw rd rs1 csr12) = do
  x <- getRegister rs1
  when (rd /= 0) (do
    y <- getCSR (lookupCSR csr12)
    setRegister rd ((fromIntegral:: MachineInt -> t) y))
  setCSR (lookupCSR csr12) x
execute (Csrrs rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .|. ((fromIntegral:: t -> MachineInt) mask)))
execute (Csrrc rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .&. ((fromIntegral:: t -> MachineInt) mask)))
execute (Csrrwi rd zimm csr12) = do
  when (rd /= 0) (do
    val <- getCSR (lookupCSR csr12)
    setRegister rd ((fromIntegral:: MachineInt -> t) val))
  setCSR (lookupCSR csr12) zimm
execute (Csrrsi rd zimm csr12) = do
  val <- getCSR (lookupCSR csr12)
  setRegister rd ((fromIntegral:: MachineInt -> t) val)
  when (zimm /= 0) (setCSR (lookupCSR csr12) (val .|. zimm))
execute (Csrrci rd zimm csr12) = do
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
  mpp <- getCSRField Field.MPP
  setCSRField Field.MPP (encodePrivMode User)
  setPrivMode (decodePrivMode mpp)
  mpie <- getCSRField Field.MPIE
  setCSRField Field.MPIE 1
  setCSRField Field.MIE mpie
  mepc <- getCSRField Field.MEPC
  setPC ((fromIntegral:: MachineInt -> t) mepc)
-- TODO: Sret, Uret
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
