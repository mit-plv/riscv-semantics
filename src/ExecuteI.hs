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

execute :: forall p t. (RiscvProgram p t) => InstructionI -> p ()
-- begin ast
execute (Lui rd imm20) = do
  setRegister rd (fromImm imm20)
execute (Auipc rd oimm20) = do
  pc <- getPC
  setRegister rd (fromImm oimm20 + pc)
execute (Jal rd jimm20) = do
  pc <- getPC
  let newPC = pc + (fromImm jimm20)
  if (remu newPC 4 /= 0)
    then raiseException 0 0
    else (do
      setRegister rd (pc + 4)
      setPC newPC)
execute (Jalr rd rs1 oimm12) = do
  x <- getRegister rs1
  pc <- getPC
  let newPC = (x + fromImm oimm12) .&. (complement 1)
  if (remu newPC 4 /= 0)
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
    if (remu newPC 4 /= 0)
      then raiseException 0 0
      else setPC newPC)
execute (Bne rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x /= y) (do
    let addr = (pc + fromImm sbimm12)
    if (remu addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Blt rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x < y) (do
    let addr = (pc + fromImm sbimm12)
    if (remu addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bge rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x >= y) (do
    let addr = (pc + fromImm sbimm12)
    if (remu addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bltu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when ((ltu x y)) (do
    let addr = (pc + fromImm sbimm12)
    if (remu addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Bgeu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (not (ltu x y)) (do
    let addr = (pc + fromImm sbimm12)
    if (remu addr 4 /= 0)
      then raiseException 0 0
      else setPC addr)
execute (Lb rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 1 (a + fromImm oimm12)
  x <- loadByte addr
  setRegister rd (int8ToReg x)
execute (Lh rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 2 (a + fromImm oimm12)
  x <- loadHalf addr
  setRegister rd (int16ToReg x)
execute (Lw rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 4 (a + fromImm oimm12)
  x <- loadWord addr
  setRegister rd (int32ToReg x)
execute (Lbu rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 1 (a + fromImm oimm12)
  x <- loadByte addr
  setRegister rd (uInt8ToReg x)
execute (Lhu rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 2 (a + fromImm oimm12)
  x <- loadHalf addr
  setRegister rd (uInt16ToReg x)
execute (Sb rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 1 (a + fromImm simm12)
  x <- getRegister rs2
  storeByte addr (regToInt8 x)
execute (Sh rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 2 (a + fromImm simm12)
  x <- getRegister rs2
  storeHalf addr (regToInt16 x)
execute (Sw rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 4 (a + fromImm simm12)
  x <- getRegister rs2
  storeWord addr (regToInt32 x)
execute (Addi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x + fromImm imm12)
execute (Slti rd rs1 imm12) = do
  x <- getRegister rs1
  let val = (if x < (fromImm imm12) then 1 else 0)
  setRegister rd val
execute (Sltiu rd rs1 imm12) = do
  x <- getRegister rs1
  let val = (if (ltu x (fromImm imm12)) then 1 else 0)
  setRegister rd val
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
  setRegister rd (sll x shamt6)
execute (Srli rd rs1 shamt6) = do
  x <- getRegister rs1
  setRegister rd (srl x shamt6)
execute (Srai rd rs1 shamt6) = do
  x <- getRegister rs1
  setRegister rd (sra x shamt6)
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
  let val = if x < y then 1 else 0
  setRegister rd val
execute (Sltu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let val = if ltu x y then 1 else 0
  setRegister rd val
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
execute (Fence pred succ) = return () -- TODO
execute Fence_i = return () -- TODO
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
