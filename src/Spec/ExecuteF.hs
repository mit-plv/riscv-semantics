{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteF where
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Spec.VirtualMemory
import qualified Spec.CSRField as Field
import Data.Bits
import Data.Word
import Data.Int
import SoftFloat
import Prelude hiding (isNaN)

canonicalNaN = 0x7fc00000 :: Int32
negativeZero = 0x80000000 :: Int32
positiveZero = 0x00000000 :: Int32
negativeInfinity = 0xff800000 :: Int32
positiveInfinity = 0x7f800000 :: Int32

intToRoundingMode :: MachineInt -> Maybe RoundingMode
intToRoundingMode 0 = Just RoundNearEven
intToRoundingMode 1 = Just RoundMinMag
intToRoundingMode 2 = Just RoundMin
intToRoundingMode 3 = Just RoundMax
intToRoundingMode 4 = Just RoundNearMaxMag
intToRoundingMode _ = Nothing

isNaN :: (Integral t) => t -> Bool
isNaN x = not notNaN
  where Result notNaN _ = f32Eq (fromIntegral x :: Word32) (fromIntegral x :: Word32)

getRoundMode :: (RiscvMachine p t) => MachineInt -> p RoundingMode
getRoundMode rm = do
  frm <- (if rm == 7 then
            getCSRField Field.FRM
          else
            return rm)
  case intToRoundingMode frm of
    Just roundMode -> return roundMode
    Nothing -> raiseException 0 2

boolBit :: (Bits t) => Int -> Bool -> t
boolBit i b = if b then bit i else zeroBits

updateFFlags :: (RiscvMachine p t) => ExceptionFlags -> p ()
updateFFlags (ExceptionFlags inexact underflow overflow infinite invalid) = do
  flags <- getCSRField Field.FFlags
  let flags' = flags .|. boolBit 0 inexact .|. boolBit 1 underflow .|.
               boolBit 2 overflow .|. boolBit 3 infinite .|. boolBit 4 invalid
  setCSRField Field.FFlags flags'

runFPUnary :: (RiscvMachine p t) => (RoundingMode -> Word32 -> F32Result) -> RoundingMode -> Int32 -> p Int32
runFPUnary f roundMode x = do
  let Result y flags = f roundMode (fromIntegral x)
  updateFFlags flags
  if (invalid flags) then
    return canonicalNaN
  else
    return (fromIntegral y)

runFPBinary :: (RiscvMachine p t) => (RoundingMode -> Word32 -> Word32 -> F32Result) -> RoundingMode -> Int32 -> Int32 -> p Int32
runFPBinary f roundMode x y = do
  let Result z flags = f roundMode (fromIntegral x) (fromIntegral y)
  updateFFlags flags
  if (invalid flags) then
    return canonicalNaN
  else
    return (fromIntegral z)

execute :: forall p t. (RiscvMachine p t) => InstructionF -> p ()
execute (Flw rd rs1 oimm12) = do
  a <- getRegister rs1
  addr <- translate Load 4 (a + fromImm oimm12)
  x <- loadWord addr
  setFPRegister rd x
execute (Fsw rs1 rs2 simm12) = do
  a <- getRegister rs1
  addr <- translate Store 4 (a + fromImm simm12)
  x <- getFPRegister rs2
  storeWord addr x
execute (Fmadd_s rd rs1 rs2 rs3 rm) = do
  roundMode <- getRoundMode rm
  v <- getFPRegister rs1
  w <- getFPRegister rs2
  x <- getFPRegister rs3
  y <- runFPBinary f32Mul roundMode v w
  z <- runFPBinary f32Add roundMode y x
  setFPRegister rd z
execute (Fmsub_s rd rs1 rs2 rs3 rm) = do
  roundMode <- getRoundMode rm
  v <- getFPRegister rs1
  w <- getFPRegister rs2
  x <- getFPRegister rs3
  y <- runFPBinary f32Mul roundMode v w
  z <- runFPBinary f32Sub roundMode y x
  setFPRegister rd z
execute (Fnmsub_s rd rs1 rs2 rs3 rm) = do
  roundMode <- getRoundMode rm
  v <- getFPRegister rs1
  w <- getFPRegister rs2
  x <- getFPRegister rs3
  y <- runFPBinary f32Mul roundMode v w
  z <- runFPBinary f32Sub roundMode x y
  setFPRegister rd z
execute (Fnmadd_s rd rs1 rs2 rs3 rm) = do
  roundMode <- getRoundMode rm
  v <- getFPRegister rs1
  w <- getFPRegister rs2
  x <- getFPRegister rs3
  y <- runFPBinary f32Mul roundMode v w
  z <- runFPBinary f32Sub roundMode (xor y (bit 31)) x
  setFPRegister rd z
execute (Fadd_s rd rs1 rs2 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  z <- runFPBinary f32Add roundMode x y
  setFPRegister rd z
execute (Fsub_s rd rs1 rs2 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  z <- runFPBinary f32Sub roundMode x y
  setFPRegister rd z
execute (Fmul_s rd rs1 rs2 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  z <- runFPBinary f32Mul roundMode x y
  setFPRegister rd z
execute (Fdiv_s rd rs1 rs2 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  z <- runFPBinary f32Div roundMode x y
  setFPRegister rd z
execute (Fsqrt_s rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  y <- runFPUnary f32Sqrt roundMode x
  setFPRegister rd y
execute (Fsgnj_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  setFPRegister rd (bitSlice x 0 31 .|. (y .&. bit 31))
execute (Fsgnjn_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  setFPRegister rd (bitSlice x 0 31 .|. (complement y .&. bit 31))
execute (Fsgnjx_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  setFPRegister rd (x `xor` (y .&. bit 31))
execute (Fmin_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  let Result z flags = f32LtQuiet (fromIntegral x :: Word32) (fromIntegral y :: Word32)
  updateFFlags flags
  -- Special cases and other sheNaNigans. Current behavior (v2.3-draft) differs
  -- from v2.2 of spec.
  let result | x == negativeZero && y == positiveZero = x
             | isNaN x && isNaN y = canonicalNaN
             | isNaN y = x
             | isNaN x = y
             | z = x
             | otherwise = y
  setFPRegister rd result
execute (Fmax_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  let Result z flags = f32LtQuiet (fromIntegral x :: Word32) (fromIntegral y :: Word32)
  updateFFlags flags
  -- Special cases and other sheNaNigans. Current behavior (v2.3-draft) differs
  -- from v2.2 of spec.
  let result | x == negativeZero && y == positiveZero = y
             | isNaN x && isNaN y = canonicalNaN
             | isNaN y = x
             | isNaN x = y
             | z = y
             | otherwise = x
  setFPRegister rd result
execute (Fcvt_w_s rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  let Result y flags = f32ToI32 roundMode (fromIntegral x :: Word32)
  updateFFlags flags
  -- Special case for the softfloat library.
  let result | isNaN x || (y == 2^31 && not (testBit x 31)) = 2^31 - 1
             | otherwise = y
  setRegister rd (fromIntegral result)
execute (Fcvt_wu_s rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getFPRegister rs1
  let Result y flags = f32ToUi32 roundMode (fromIntegral x :: Word32)
  updateFFlags flags
  -- Another special case for the softfloat library.
  let result | not (isNaN x) && testBit x 31 = 0
             | otherwise = y
  setRegister rd (fromIntegral (fromIntegral result :: Int32))
execute (Fmv_x_w rd rs1) = do
  x <- getFPRegister rs1
  setRegister rd (fromIntegral x)
execute (Feq_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  let Result z flags = f32Eq (fromIntegral x :: Word32) (fromIntegral y :: Word32)
  updateFFlags flags
  let result = fromIntegral (fromEnum z)
  setRegister rd result
execute (Flt_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  let Result z flags = f32Lt (fromIntegral x :: Word32) (fromIntegral y :: Word32)
  updateFFlags flags
  let result = fromIntegral (fromEnum z)
  setRegister rd result
execute (Fle_s rd rs1 rs2) = do
  x <- getFPRegister rs1
  y <- getFPRegister rs2
  let Result z flags = f32Le (fromIntegral x :: Word32) (fromIntegral y :: Word32)
  updateFFlags flags
  let result = fromIntegral (fromEnum z)
  setRegister rd result
execute (Fclass_s rd rs1) = do
  x <- getFPRegister rs1
  let special = bitSlice x 23 31 == 2^8 - 1
  let result | x == negativeInfinity = bit 0
             | x == negativeZero = bit 3
             | x == positiveZero = bit 4
             | x == positiveInfinity = bit 7
             | testBit x 22 && special = bit 9 -- quiet NaN
             | special = bit 8 -- signaling NaN
             | testBit x 22 && testBit x 31 = bit 2 -- negative subnormal
             | testBit x 22 = bit 5 -- positive subnormal
             | testBit x 31 = bit 1 -- negative normal
             | otherwise = bit 6 -- positive normal
  setRegister rd result
execute (Fcvt_s_w rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getRegister rs1
  let Result y flags = i32ToF32 roundMode (fromIntegral x :: Int32)
  updateFFlags flags
  setFPRegister rd (fromIntegral y)
execute (Fcvt_s_wu rd rs1 rm) = do
  roundMode <- getRoundMode rm
  x <- getRegister rs1
  let Result y flags = ui32ToF32 roundMode (fromIntegral x :: Word32)
  updateFFlags flags
  setFPRegister rd (fromIntegral y)
execute (Fmv_w_x rd rs1) = do
  x <- getRegister rs1
  setFPRegister rd (fromIntegral x)
execute inst = error $ "dispatch bug: " ++ show inst
