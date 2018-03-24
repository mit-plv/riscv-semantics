{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM64 where
import Decode
import Program
import Utility
import Control.Monad

execute :: forall p t. (RiscvProgram p t) => InstructionM64 -> p ()
-- begin ast
execute (Mulw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (x * y))
execute (Divw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minSigned && y == -1 = x
        | y == 0 = -1
        | otherwise = quot x y
    in setRegister rd (s32 q)
execute (Divuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxUnsigned
        | otherwise = divu x y
    in setRegister rd (s32 q)
execute (Remw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minSigned && y == -1 = 0
        | y == 0 = x
        | otherwise = rem x y
    in setRegister rd (s32 r)
execute (Remuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = remu x y
    in setRegister rd (s32 r)
-- end ast
