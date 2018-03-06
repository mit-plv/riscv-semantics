{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteI64 where
import Decode
import Program
import Utility
import VirtualMemory
import Data.Bits
import Data.Int
import Control.Monad

execute :: forall p t. (RiscvProgram p t, MonadPlus p) => Instruction -> p ()
execute (Lwu rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 4 (a + fromImm oimm12)
    (\addr -> do
        x <- loadWord addr
        setRegister rd (uInt32ToReg x))
execute (Ld rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 8 (a + fromImm oimm12)
    (\addr -> do
        x <- loadDouble addr
        setRegister rd (int64ToReg x))
execute (Sd rs1 rs2 simm12) = do
  a <- getRegister rs1
  withTranslation Store 8 (a + fromImm simm12)
    (\addr -> do
        x <- getRegister rs2
        storeDouble addr (regToInt64 x))
execute (Addiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (x + fromImm imm12))
execute (Slliw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (sll x (regToShamt5 (fromImm shamt5 :: t))))
execute (Srliw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (srl (u32 x) (regToShamt5 (fromImm shamt5 :: t))))
execute (Sraiw rd rs1 shamt5) = do
  x <- getRegister rs1
  setRegister rd (s32 (sra (s32 x) (regToShamt5 (fromImm shamt5 :: t))))
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
execute _ = mzero
