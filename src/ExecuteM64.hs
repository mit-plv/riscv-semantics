{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM64 where
import Decode
import Program
import Utility
import Control.Monad

execute :: forall p t u. (RiscvProgram p t u, MonadPlus p) => Instruction -> p ()
execute (Mulw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 (x * y))
execute (Divw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minBound && y == -1 = x
        | y == 0 = -1
        | otherwise = quot x y
    in setRegister rd (s32 q)
execute (Divuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxBound::u
        | otherwise = div (unsigned x) (unsigned y)
    in setRegister rd (s32 q)
execute (Remw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minBound && y == -1 = 0
        | y == 0 = x
        | otherwise = rem x y
    in setRegister rd (s32 r)
execute (Remuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = (fromIntegral:: u -> t) (rem (unsigned x) (unsigned y))
    in setRegister rd (s32 r)
execute _ = mzero
