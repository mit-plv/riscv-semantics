{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM where
import Decode
import Program
import Utility
import Control.Monad
import Prelude

execute :: forall p t u. (RiscvProgram p t u, MonadPlus p) => Instruction -> p ()
-- begin ast
execute (Mul rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (x * y)
execute (Mulh rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits (((fromIntegral:: t -> Integer) x) * ((fromIntegral:: t -> Integer) y))::t)
execute (Mulhsu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits (((fromIntegral:: t -> Integer) x) * ((fromIntegral:: u -> Integer) (unsigned y)))::t)
execute (Mulhu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (highBits (((fromIntegral:: u -> Integer) (unsigned x)) * ((fromIntegral:: u -> Integer)  (unsigned y)))::t)
execute (Div rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minBound && y == -1 = x
        | y == 0 = -1
        | otherwise = quot x y
    in setRegister rd q
execute (Divu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxBound::u
        | otherwise = div (unsigned x) (unsigned y)
    in setRegister rd q
execute (Rem rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minBound && y == -1 = 0
        | y == 0 = x
        | otherwise = rem x y
    in setRegister rd r
execute (Remu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = (fromIntegral:: u -> t) (rem (unsigned x) (unsigned y))
    in setRegister rd r
-- end ast
execute _ = mzero
