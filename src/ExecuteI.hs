{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteI where
import Decode
import Program
import Utility
import Data.Bits
import Data.Word
import Control.Monad
import Prelude

execute :: forall p t u. (RiscvProgram p t u) => Instruction -> p ()
-- begin ast
execute (Lui rd imm20) = setRegister rd imm20
execute (Auipc rd imm20) = do
  pc <- getPC
  setRegister rd (fromIntegral imm20 + pc)
execute (Jal rd jimm20) = do
  pc <- getPC
  setRegister rd (fromIntegral pc + 4)
  setPC (pc + (fromIntegral jimm20))
  pc <- getPC
  when (mod pc 4 /= 0) (raiseException 0 0)
execute (Jalr rd rs1 oimm12) = do
  x <- getRegister rs1
  pc <- getPC
  setPC (x + fromIntegral oimm12)
  setRegister rd (fromIntegral pc + 4)
  pc <- getPC
  when (mod pc 4 /= 0) (raiseException 0 0)
execute (Beq rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x == y) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Bne rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x /= y) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Blt rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x < y) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Bge rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when (x > y) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Bltu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when ((unsigned x) < (unsigned y)) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Bgeu rs1 rs2 sbimm12) = do
  x <- getRegister rs1
  y <- getRegister rs2
  pc <- getPC
  when ((unsigned x) > (unsigned y)) $ do
    setPC (pc + fromIntegral sbimm12)
    pc <- getPC
    when (mod pc 4 /= 0) (raiseException 0 0)
execute (Lb rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- loadByte (a + fromIntegral oimm12)
  setRegister rd x
execute (Lh rd rs1 oimm12) = do
  a <- getRegister rs1
  let addr = a + fromIntegral oimm12
  if mod addr 2 /= 0
    then raiseException 0 4
    else do
    x <- loadHalf addr
    setRegister rd x
execute (Lw rd rs1 oimm12) = do
  a <- getRegister rs1
  let addr = a + fromIntegral oimm12
  if mod addr 4 /= 0
    then raiseException 0 4
    else do
    x <- loadWord addr
    setRegister rd x
execute (Lbu rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- loadByte (a + fromIntegral oimm12)
  setRegister rd (unsigned x)
execute (Lhu rd rs1 oimm12) = do
  a <- getRegister rs1
  let addr = a + fromIntegral oimm12
  if mod addr 2 /= 0
    then raiseException 0 4
    else do
    x <- loadHalf addr
    setRegister rd (unsigned x)
execute (Sb rs1 rs2 simm12) = do
  a <- getRegister rs1
  x <- getRegister rs2
  storeByte (a + fromIntegral simm12) x
execute (Sh rs1 rs2 simm12) = do
  a <- getRegister rs1
  let addr = a + fromIntegral simm12
  if mod addr 2 /= 0
    then raiseException 0 6
    else do
    x <- getRegister rs2
    storeHalf addr x
execute (Sw rs1 rs2 simm12) = do
  a <- getRegister rs1
  let addr = a + fromIntegral simm12
  if mod addr 4 /= 0
    then raiseException 0 6
    else do
    x <- getRegister rs2
    storeWord addr x
execute (Addi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x + fromIntegral imm12)
execute (Slti rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (if x < fromIntegral imm12 then 1 else 0)
execute (Sltiu rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (if (unsigned x) < (fromIntegral imm12 :: u) then 1 else 0)
execute (Xori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (xor x (fromIntegral imm12))
execute (Ori rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x .|. (fromIntegral imm12))
execute (Andi rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (x .&. (fromIntegral imm12))
execute (Slli rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftL x (shiftBits (fromIntegral imm12 :: t)))
execute (Srli rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftR (unsigned x) (shiftBits (fromIntegral imm12 :: t)))
execute (Srai rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (shiftR x (shiftBits (fromIntegral imm12 :: t)))
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
  setRegister rd (shiftL x (shiftBits y))
execute (Slt rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if x < y then 1 else 0)
execute (Sltu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (if (unsigned x) < (unsigned y) then 1 else 0)
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
  setRegister rd (shiftR (unsigned x) (shiftBits y))
execute (Sra rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (shiftR x (shiftBits y))
execute (And rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x .&. y)
-- end ast
execute _ = mzero
-- TODO: Fence/Fence.i?
