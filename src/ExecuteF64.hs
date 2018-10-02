{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteF64 where
import Decode
import Program
import ExecuteF (getRoundMode, updateFFlags, isNaN)
import Data.Int
import Data.Word
import Data.Bits
import SoftFloat
import Prelude hiding (isNaN)

execute :: forall p t. (RiscvProgram p t) => InstructionF64 -> p ()
execute (Fcvt_l_s rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  let Result y flags = f32ToI64 roundMode (fromIntegral x :: Word32)
  updateFFlags flags
  -- Special case for the softfloat library.
  let result | isNaN x || (y == 2^63 && not (testBit x 31)) = 2^63 - 1
             | otherwise = y
  setRegister rd (fromIntegral result)
execute (Fcvt_lu_s rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  let Result y flags = f32ToUi64 roundMode (fromIntegral x :: Word32)
  updateFFlags flags
  -- Another special case for the softfloat library.
  let result | not (isNaN x) && testBit x 31 = 0
             | otherwise = y
  setRegister rd (fromIntegral (fromIntegral result :: Int64))
execute (Fcvt_s_l rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getRegister rs1
  let Result y flags = i64ToF32 roundMode (fromIntegral x :: Int64)
  updateFFlags flags
  setFPRegister rd (fromIntegral y)
execute (Fcvt_s_lu rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getRegister rs1
  let Result y flags = ui64ToF32 roundMode (fromIntegral x :: Word64)
  updateFFlags flags
  setFPRegister rd (fromIntegral y)
execute inst = error $ "dispatch bug: " ++ show inst
