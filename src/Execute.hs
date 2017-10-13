module Execute where
import Decode
import Program
import CSR hiding (decode)
import ExecuteI as I
import ExecuteI64 as I64
import ExecuteM as M
import ExecuteM64 as M64
import ExecuteCSR as CSR
import Control.Monad

execute :: (RiscvProgram p t u) => Instruction -> p ()
execute InvalidInstruction = do
  pc <- getPC
  addr <- loadCSR mtvec_addr
  storeCSR mepc_addr pc
  storeCSR mcause_addr 2 -- Illegal instruction
  setPC addr
execute inst = msum (map (\f -> f inst) [I.execute, I64.execute, M.execute, M64.execute, CSR.execute])
