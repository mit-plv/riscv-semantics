{-# LANGUAGE ScopedTypeVariables #-}
module Execute where
import Decode
import Program
import Data.Bits
import Control.Monad
import Control.Monad.State

execute :: forall p t u. (RiscvProgram p t u) => Instruction -> p ()
execute (Lui rd imm20) = setRegister rd (fromIntegral imm20)
execute (Auipc rd imm20) = do
  pc <- getPC
  setRegister rd (fromIntegral imm20 + pc)
execute (Jal rd jimm20) = do
  pc <- getPC
  setRegister rd (fromIntegral pc + 4)
  setPC (pc + (fromIntegral jimm20))
execute (Jalr rd rs1 oimm12) = do
  x <- getRegister rs1
  pc <- getPC
  setPC (x + fromIntegral oimm12)
  setRegister rd (fromIntegral pc + 4)
execute (Beq rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when (x == y) $ setPC sbimm12
execute (Bne rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when (x /= y) $ setPC sbimm12
execute (Blt rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when (x < y) $ setPC sbimm12
execute (Bge rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when (x > y) $ setPC sbimm12
execute (Bltu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when ((fromIntegral x :: u) < (fromIntegral y :: u)) $ setPC sbimm12
execute (Bgeu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  when ((fromIntegral x :: u) > (fromIntegral y :: u)) $ setPC sbimm12
execute (Lb rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- load (a + fromIntegral oimm12)
  setRegister rd (s8 x)
execute (Lh rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- load (a + fromIntegral oimm12)
  setRegister rd (s16 x)
execute (Lw rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- load (a + fromIntegral oimm12)
  setRegister rd (s32 x)
execute (Lbu rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- load (a + fromIntegral oimm12)
  setRegister rd (u8 x)
execute (Lhu rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- load (a + fromIntegral oimm12)
  setRegister rd (u16 x)
execute (Sb rs1 rs2 simm12) = do
  a <- getRegister rs1
  x <- getRegister rs2
  store (a + fromIntegral simm12) (s8 x)
execute (Sh rs1 rs2 simm12) = do
  a <- getRegister rs1
  x <- getRegister rs2
  store (a + fromIntegral simm12) (s16 x)
execute (Sw rs1 rs2 simm12) = do
  a <- getRegister rs1
  x <- getRegister rs2
  store (a + fromIntegral simm12) (s32 x)
execute (Addi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x + fromIntegral imm12)
execute (Slti rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (if x < fromIntegral imm12 then 1 else 0)
execute (Sltiu rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (if (fromIntegral x :: u) < (fromIntegral imm12 :: u) then 1 else 0)
execute (Xori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (xor x (fromIntegral imm12))
execute (Ori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd ((.|.) x (fromIntegral imm12))
execute (Andi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd ((.&.) x (fromIntegral imm12))
execute (Slli rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftL x (lower5 imm12))
execute (Srli rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftR (fromIntegral x :: u) (lower5 imm12))
execute (Srai rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftR x (lower5 imm12))
execute (Add rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x + y)
execute (Sub rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x - y)
execute (Sll rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (shiftL x (lower5 y))
execute (Slt rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if x < y then 1 else 0)
execute (Sltu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if (fromIntegral x :: u) < (fromIntegral y :: u) then 1 else 0)
execute (Xor rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (xor x y)
execute (Or rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd ((.|.) x y)
execute (Srl rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (shiftR (fromIntegral x :: u) (lower5 y))
execute (Sra rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (shiftR x (lower5 y))
execute (And rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd ((.&.) x y)
-- TODO: Fence/Fence.i?

-- Example usage:
c = Computer32 { registers = [0,0,0,0], pc = 5, mem = [0,0,0,0] }
action = do
  execute (Lui 1 19)
  execute (Lui 2 23)
  execute (Lui 4 1)
  execute (Add 3 1 2)
  execute (Sw 4 3 0)
cp = runState action c
