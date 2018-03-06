{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteI where
import Decode
import Program
import Utility
import VirtualMemory
import Data.Bits
import Data.Int
import Data.Word
import Control.Monad
import Prelude

execute :: forall p t u. (RiscvProgram p t u, MonadPlus p) => Instruction -> p ()
-- begin ast
execute (Lui rd imm20) = setRegister rd imm20
execute (Auipc rd oimm20) = do
  pc <- getPC
  setRegister rd (fromImm oimm20 + pc)
execute (Jal rd jimm20) = do
  pc <- getPC
  let newPC = pc + (fromImm jimm20)
  if (mod newPC 4 /= 0)
    then raiseException 0 0
    else (do
      setRegister rd (pc + 4)
      setPC newPC)
execute (Jalr rd rs1 oimm12) = do
  x <- getRegister rs1
  pc <- getPC
  let newPC = x + fromImm oimm12
  if (mod newPC 4 /= 0)
    then raiseException 0 0
    else (do
      setRegister rd (pc + 4)
      setPC newPC)
execute (Beq rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x == y) (do
    let newPC = (pc + fromImm sbimm12)
    if (mod newPC 4 /= 0)
      then raiseException 0 0
      else setPC newPC)
execute (Bne rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x /= y) (do
    let addr = (pc + fromImm sbimm12)
    if (mod addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Blt rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x < y) (do
    let addr = (pc + fromImm sbimm12)
    if (mod addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bge rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x >= y) (do
    let addr = (pc + fromImm sbimm12)
    if (mod addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bltu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when ((ltu x y)) (do
    let addr = (pc + fromImm sbimm12)
    if (mod addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bgeu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (not (ltu x y)) (do
    let addr = (pc + fromImm sbimm12)
    if (mod addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Lb rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 1 (a + fromImm oimm12)
    (\addr -> do
        x <- loadByte addr
        setRegister rd x)
execute (Lh rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 2 (a + fromImm oimm12)
    (\addr -> do
        x <- loadHalf addr
        setRegister rd x)
execute (Lw rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 4 (a + fromImm oimm12)
    (\addr -> do
        x <- loadWord addr
        setRegister rd x)
execute (Lbu rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 1 (a + fromImm oimm12)
    (\addr -> do
        x <- loadByte addr
        setRegister rd (unsigned x))
execute (Lhu rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 2 (a + fromImm oimm12)
    (\addr -> do
        x <- loadHalf addr
        setRegister rd (unsigned x))
execute (Sb rs1 rs2 simm12) = do
  a <- getRegister rs1
  withTranslation Store 1 (a + fromImm simm12)
    (\addr -> do
        x <- getRegister rs2
        storeByte addr (regToInt8 x))
execute (Sh rs1 rs2 simm12) = do
  a <- getRegister rs1
  withTranslation Store 2 (a + fromImm simm12)
    (\addr -> do
        x <- getRegister rs2
        storeHalf addr (regToInt16 x))
execute (Sw rs1 rs2 simm12) = do
  a <- getRegister rs1
  withTranslation Store 4 (a + fromImm simm12)
    (\addr -> do
        x <- getRegister rs2
        storeWord addr (regToInt32 x))
execute (Addi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x + fromImm imm12)
execute (Slti rd rs1 imm12) = do
  x <- getRegister rs1 -- Why is the fromIntegral here required?
  setRegister rd (if x < (fromImm imm12) then 1 else 0)
execute (Sltiu rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (if (ltu x imm12) then 1 else 0)
execute (Xori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (xor x (fromImm imm12))
execute (Ori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x .|. (fromImm imm12))
execute (Andi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x .&. (fromImm imm12))
execute (Slli rd rs1 shamt6) = do
  x <- getRegister rs1
  setRegister rd (sll x (regToShamt (fromImm shamt6 :: t)))
execute (Srli rd rs1 shamt6) = do
  x <- getRegister rs1
  setRegister rd (srl x (regToShamt (fromImm shamt6 :: t)))
execute (Srai rd rs1 shamt6) = do
  x <- getRegister rs1
  setRegister rd (sra x (regToShamt (fromImm shamt6 :: t)))
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
  setRegister rd (sll x (regToShamt y))
execute (Slt rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if x < y then 1 else 0)
execute (Sltu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if (ltu x y) then 1 else 0)
execute (Xor rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (xor x y)
execute (Or rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x .|. y)
execute (Srl rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (srl x (regToShamt y))
execute (Sra rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (sra x (regToShamt y))
execute (And rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x .&. y)
-- end ast
execute _ = mzero
-- TODO: Fence/Fence.i?
