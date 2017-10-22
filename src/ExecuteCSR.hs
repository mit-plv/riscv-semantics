{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteCSR where
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
    y <- loadCSR (fromIntegral csr12)
    setRegister rd y)
  storeCSR (fromIntegral csr12) x
execute (Csrrs rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- loadCSR (fromIntegral csr12)
  setRegister rd val
  when (rs1 /= 0) (storeCSR (fromIntegral csr12) (val .|. (fromIntegral mask)))
execute (Csrrc rd rs1 csr12) = do
  mask <- getRegister rs1
  val <- loadCSR (fromIntegral csr12)
  setRegister rd val
  when (rs1 /= 0) (storeCSR (fromIntegral csr12) (val .&. (fromIntegral mask)))
execute (Csrrwi rd zimm csr12) = do
  when (rd /= 0) (do
    val <- loadCSR (fromIntegral csr12)
    setRegister rd val)
  storeCSR (fromIntegral csr12) zimm
execute (Csrrsi rd zimm csr12) = do
  val <- loadCSR (fromIntegral csr12)
  setRegister rd val
  when (zimm /= 0) (storeCSR (fromIntegral csr12) (val .|. (fromIntegral zimm)))
execute (Csrrci rd zimm csr12) = do
  val <- loadCSR (fromIntegral csr12)
  setRegister rd val
  when (zimm /= 0) (storeCSR (fromIntegral csr12) (val .&. (complement (fromIntegral zimm))))
-- end ast
execute _ = mzero
