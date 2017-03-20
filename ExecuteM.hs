{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM where
import Decode
import Program
import Utility
import Control.Monad

executeM :: forall p t u. (RiscvProgram p t u) => Instruction -> p ()
executeM (Mul rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x * y)
executeM (Mulh rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((fromIntegral x) * (fromIntegral y))::t)
executeM (Mulhsu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((fromIntegral x) * (fromIntegral $ unsigned y))::t)
executeM (Mulhu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((fromIntegral $ unsigned x) * (fromIntegral $ unsigned y))::t)
executeM (Div rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minBound && y == -1 = x
        | y == 0 = -1
        | otherwise = x `div` y
    in setRegister rd q
executeM (Divu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxBound::u
        | otherwise = (unsigned x) `div` (unsigned y)
    in setRegister rd q
executeM (Rem rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minBound && y == -1 = 0
        | y == 0 = x
        | otherwise = x `rem` y
    in setRegister rd r
executeM (Remu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = fromIntegral $ (unsigned x) `rem` (unsigned y)
    in setRegister rd r
executeM _ = mzero
