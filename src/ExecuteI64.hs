{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteI64 where
import Decode
import Program
import Utility
import Data.Bits
import Data.Int
import Control.Monad

execute :: forall p t u. (RiscvProgram p t u, MonadPlus p) => Instruction -> p ()
execute (Lwu rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- loadWord (a + fromIntegral oimm12)
  setRegister rd (unsigned x)
execute (Ld rd rs1 oimm12) = do
  a <- getRegister rs1
  x <- loadDouble (a + fromIntegral oimm12)
  setRegister rd x
execute (Sd rs1 rs2 simm12) = do
  a <- getRegister rs1
  x <- getRegister rs2
  storeDouble (a + fromIntegral simm12) x
execute (Addiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (x + fromIntegral imm12))
execute (Slliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftL x (shiftBits (fromIntegral imm12 :: Int32))))
execute (Srliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR (unsigned x) (shiftBits (fromIntegral imm12 :: Int32))))
execute (Sraiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR x (shiftBits (fromIntegral imm12 :: Int32))))
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
  setRegister rd (s32 (shiftL x (shiftBits (fromIntegral y :: Int32))))
execute (Srlw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR (unsigned x) (shiftBits (fromIntegral y :: Int32))))
execute (Sraw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR x (shiftBits (fromIntegral y :: Int32))))
execute _ = mzero
