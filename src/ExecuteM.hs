{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM where
import Decode
import Program
import Utility
import Control.Monad
import Prelude

execute :: forall p t. (RiscvProgram p t) => InstructionM -> p ()
-- begin ast
execute (Mul rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x * y)
execute (Mulh rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((regToZ_signed x) * (regToZ_signed y)) :: t)
execute (Mulhsu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((regToZ_signed x) * (regToZ_unsigned y)) :: t)
execute (Mulhu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits ((regToZ_unsigned x) * (regToZ_unsigned y)) :: t)
execute (Div rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minSigned && y == -1 = x
        | y == 0 = -1
        | otherwise = quot x y
    in setRegister rd q
execute (Divu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxUnsigned
        | otherwise = divu x y
    in setRegister rd q
execute (Rem rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minSigned && y == -1 = 0
        | y == 0 = x
        | otherwise = rem x y
    in setRegister rd r
execute (Remu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = remu x y
    in setRegister rd r
-- end ast

