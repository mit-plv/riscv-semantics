{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteI64 where
import Decode
import Program
import Utility
import VirtualMemory
import Data.Bits
import Data.Int
import Control.Monad

execute :: forall p t u. (RiscvProgram p t u, MonadPlus p) => Instruction -> p ()
execute (Lwu rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 4 (a + fromImm oimm12)
  x <- loadWord addr
  setRegister rd (unsigned x)
execute (Ld rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 8 (a + fromImm oimm12)
  x <- loadDouble addr
  setRegister rd x
execute (Sd rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 8 (a + fromImm simm12)
  x <- getRegister rs2
  storeDouble addr (regToInt64 x)
execute (Addiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (x + fromImm imm12))
execute (Slliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftL x (regToShamt5 (fromImm imm12 :: t))))
execute (Srliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR (u32 x) (regToShamt5 (fromImm imm12 :: t))))
execute (Sraiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR (s32 x) (regToShamt5 (fromImm imm12 :: t))))
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
  setRegister rd (s32 (shiftL x (regToShamt5 y)))
execute (Srlw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR (u32 x) (regToShamt5 y)))
execute (Sraw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR x (regToShamt5 y)))
execute _ = mzero
