{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteA where
import Decode
import Program
import Utility
import VirtualMemory
import Control.Monad
import Data.Bits
import Prelude

execute :: forall p t. (RiscvProgram p t) => InstructionA -> p ()
-- begin ast
execute (Lr_w rd rs1 aqrl) = do
  a <- getRegister rs1
  addr <- translate Load 4 a
  makeReservation addr
  x <- loadWord addr
  setRegister rd (int32ToReg x)
execute (Sc_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  valid <- checkReservation addr
  if valid
    then do
    x <- getRegister rs2
    storeWord addr (regToInt32 x)
    setRegister rd 0
    else setRegister rd 1
-- TODO: Eventually stop cheating.
execute (Amoswap_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 y)
  setRegister rd (int32ToReg x)
execute (Amoadd_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x + y))
  setRegister rd (int32ToReg x)
execute (Amoand_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x .&. y))
  setRegister rd (int32ToReg x)
execute (Amoor_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x .|. y))
  setRegister rd (int32ToReg x)
execute (Amoxor_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (xor (int32ToReg x) y))
  setRegister rd (int32ToReg x)
execute (Amomax_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (if x > (regToInt32 y) then x else regToInt32 y))
  setRegister rd (int32ToReg x)
execute (Amomaxu_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (if ltu (regToInt32 y) x then x else regToInt32 y))
  setRegister rd (int32ToReg x)
execute (Amomin_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (if x < (regToInt32 y) then x else (regToInt32 y)))
  setRegister rd (int32ToReg x)
execute (Amominu_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  y <- getRegister rs2
  storeWord addr (regToInt32 (if ltu x (regToInt32 y) then x else (regToInt32 y)))
  setRegister rd (int32ToReg x)
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
