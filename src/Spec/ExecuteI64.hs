{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteI64 where
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Spec.VirtualMemory
import Data.Bits
import Data.Int
import Control.Monad

execute :: forall p t. (RiscvMachine p t) => InstructionI64 -> p ()
-- begin ast
execute (Lwu rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 4 (a + fromImm oimm12)
  x <- loadWord addr
  setRegister rd (uInt32ToReg x)
execute (Ld rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 8 (a + fromImm oimm12)
  x <- loadDouble addr
  setRegister rd (int64ToReg x)
execute (Sd rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 8 (a + fromImm simm12)
  x <- getRegister rs2
  storeDouble addr (regToInt64 x)
execute (Addiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (x + fromImm imm12))
execute (Slliw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (sll x shamt5))
execute (Srliw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (srl (u32 x) shamt5))
execute (Sraiw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (sra (s32 x) shamt5))
execute (Addw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (x + y))
execute (Subw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (x - y))
execute (Sllw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (sll x (regToShamt5 y)))
execute (Srlw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (srl (u32 x) (regToShamt5 y)))
execute (Sraw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (sra (s32 x) (regToShamt5 y)))
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
