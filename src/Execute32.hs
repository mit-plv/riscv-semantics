module Execute32 where
import Decode
import Program
import CSR hiding (decode)
import ExecuteI as I
import ExecuteM as M
import ExecuteCSR as CSR
import Control.Monad

execute :: (RiscvProgram p t u) => Instruction -> p ()
execute InvalidInstruction = do
  pc <- getPC
  addr <- loadCSR mtvec_addr
  storeCSR mepc_addr pc
  storeCSR mcause_addr 2 -- Illegal instruction
  setPC addr
execute inst = msum (map (\f -> f inst) [I.execute, M.execute, CSR.execute])
