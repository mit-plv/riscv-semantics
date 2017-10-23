{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteCSR where
import CSR
import CSRSpec
import Decode
import Program
import Utility
import Data.Bits
import Control.Monad

execute :: forall p t u. (RiscvProgram p t u) => Instruction -> p ()
-- begin ast
execute (Csrrw rd rs1 csr12) = do
  x <- getRegister rs1
  when (rd /= 0) (do
    y <- getCSR (lookupCSR csr12)
    setRegister rd y)
  setCSR (lookupCSR csr12) x
execute (Csrrs rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd val
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .|. (fromIntegral mask)))
execute (Csrrc rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- getCSR (lookupCSR csr12)
  setRegister rd val
  when (rs1 /= 0) (setCSR (lookupCSR csr12) (val .&. (fromIntegral mask)))
execute (Csrrwi rd zimm csr12) = do
  when (rd /= 0) (do
    val <- getCSR (lookupCSR csr12)
    setRegister rd val)
  setCSR (lookupCSR csr12) zimm
execute (Csrrsi rd zimm csr12) = do
  val <- getCSR (lookupCSR csr12)
  setRegister rd val
  when (zimm /= 0) (setCSR (lookupCSR csr12) (val .|. (fromIntegral zimm)))
execute (Csrrci rd zimm csr12) = do
  val <- getCSR (lookupCSR csr12)
  setRegister rd val
  when (zimm /= 0) (setCSR (lookupCSR csr12) (val .&. (complement zimm)))
-- end ast
execute _ = mzero
