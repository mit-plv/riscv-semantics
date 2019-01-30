{-# LANGUAGE ScopedTypeVariables #-}
module Spec.ExecuteA64 where
import Spec.Decode
import Spec.Machine
import Utility.Utility
import Spec.VirtualMemory
import Control.Monad
import Data.Bits
import Prelude

execute :: forall p t. (RiscvMachine p t) => InstructionA64 -> p ()
-- begin ast
execute (Lr_d rd rs1 aqrl) = do
  a <- getRegister rs1
  addr <- translate Load 8 a
  makeReservation addr
  x <- loadDouble addr
  setRegister rd (int64ToReg x)
execute (Sc_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 8 a
  valid <- checkReservation addr
  if valid
    then do
    x <- getRegister rs2
    storeDouble addr (regToInt64 x)
    setRegister rd 0
    else setRegister rd 1
-- TODO: Eventually stop cheating.
execute (Amoswap_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 y)
execute (Amoadd_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (int64ToReg x + y))
execute (Amoand_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (int64ToReg x .&. y))
execute (Amoor_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (int64ToReg x .|. y))
execute (Amoxor_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (xor (int64ToReg x) y))
execute (Amomax_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (if x > (regToInt64 y) then x else regToInt64 y))
execute (Amomaxu_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (if ltu (regToInt64 y) x then x else regToInt64 y))
execute (Amomin_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (if x < (regToInt64 y) then x else (regToInt64 y)))
execute (Amominu_d rd rs1 rs2 aqrl) = do
  a <- getRegister rs1
  addr <- translate Store 4 a
  x <- loadDouble addr
  y <- getRegister rs2
  setRegister rd (int64ToReg x)
  storeDouble addr (regToInt64 (if ltu x (regToInt64 y) then x else (regToInt64 y)))
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
