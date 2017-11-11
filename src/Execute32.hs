module Execute32 where
import Decode
import Program
import CSR hiding (decode)
import qualified CSRField as Field
import ExecuteI as I
import ExecuteM as M
import ExecuteCSR as CSR
import Control.Monad
import Prelude

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
  msum (map (\f -> f inst) [I.execute])
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
