{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteA where
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Spec.VirtualMemory
import Control.Monad
import Data.Bits
import Prelude

execute :: forall p t. (RiscvMachine p t) => InstructionA -> p ()
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
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 y)
execute (Amoadd_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x + y))
execute (Amoand_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x .&. y))
execute (Amoor_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (int32ToReg x .|. y))
execute (Amoxor_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (xor (int32ToReg x) y))
execute (Amomax_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (if x > (regToInt32 y) then x else regToInt32 y))
execute (Amomaxu_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (if ltu (regToInt32 y) x then x else regToInt32 y))
execute (Amomin_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (if x < (regToInt32 y) then x else (regToInt32 y)))
execute (Amominu_w rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadWord addr
  setRegister rd (int32ToReg x)
  y <- getRegister rs2
  storeWord addr (regToInt32 (if ltu x (regToInt32 y) then x else (regToInt32 y)))
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
