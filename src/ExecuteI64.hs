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
  withTranslation Load 4 (a + (fromIntegral:: MachineInt -> t) oimm12)
    (\addr -> do
        x <- loadWord addr
        setRegister rd (unsigned x))
execute (Ld rd rs1 oimm12) = do
  a <- getRegister rs1
  withTranslation Load 8 (a + (fromIntegral:: MachineInt -> t) oimm12)
    (\addr -> do
        x <- loadDouble addr
        setRegister rd x)
execute (Sd rs1 rs2 simm12) = do
  a <- getRegister rs1
  withTranslation Store 8 (a + (fromIntegral:: MachineInt -> t) simm12)
    (\addr -> do
        x <- getRegister rs2
        storeDouble addr ((fromIntegral:: t -> Int64) x))
execute (Addiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (x + (fromIntegral:: MachineInt -> t) imm12))
execute (Slliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftL x (shiftBits ((fromIntegral:: MachineInt -> Int32) imm12))))
execute (Srliw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR (u32 x) (shiftBits ((fromIntegral:: MachineInt -> Int32) imm12))))
execute (Sraiw rd rs1 imm12) = do
  x <- getRegister rs1
  setRegister rd (s32 (shiftR (s32 x) (shiftBits ((fromIntegral:: MachineInt -> Int32) imm12))))
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
  setRegister rd (s32 (shiftL x (shiftBits ((fromIntegral:: t -> Int32) y))))
execute (Srlw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR (u32 x) (shiftBits ((fromIntegral:: t -> Int32) y))))
execute (Sraw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (shiftR x (shiftBits ((fromIntegral:: t -> Int32) y))))
execute _ = mzero
