{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteM64 where
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Control.Monad

execute :: forall p t. (RiscvMachine p t) => InstructionM64 -> p ()
-- begin ast
execute (Mulw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (x * y))
execute (Divw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let a = s32 x
      b = s32 y
      q | a == minSigned32 && b == -1 = a
        | b == 0 = -1
        | otherwise = quot a b
    in setRegister rd (s32 q)
execute (Divuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let a = u32 x
      b = u32 y
      q | b == 0 = maxUnsigned
        | otherwise = divu a b
    in setRegister rd (s32 q)
execute (Remw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let a = s32 x
      b = s32 y
      r | a == minSigned32 && b == -1 = 0
        | b == 0 = a
        | otherwise = rem a b
    in setRegister rd (s32 r)
execute (Remuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let a = u32 x
      b = u32 y
      r | b == 0 = a
        | otherwise = remu a b
    in setRegister rd (s32 r)
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
