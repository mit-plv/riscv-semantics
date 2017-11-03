module Execute where
import Decode
import Program
import CSR
import qualified CSRField as Field
import ExecuteI as I
import ExecuteI64 as I64
import ExecuteM as M
import ExecuteM64 as M64
import ExecuteCSR as CSR
import Control.Monad

execute :: (RiscvProgram p t u) => Instruction -> p ()
execute InvalidInstruction = do
  pc <- getPC
  addr <- getCSRField Field.MTVecBase
  setCSRField Field.MEPC pc
  setCSRField Field.MCauseInterrupt 0 -- Not an interrupt
  setCSRField Field.MCauseCode 2 -- Illegal instruction
  setPC (addr * 4)
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
execute inst = do
  msum (map (\f -> f inst) [I.execute, I64.execute, M.execute, M64.execute, CSR.execute])
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
