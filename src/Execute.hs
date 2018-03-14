module Execute where
import Decode
import Program
import qualified CSRField as Field
import ExecuteI as I
import ExecuteI64 as I64
import ExecuteM as M
import ExecuteM64 as M64
import ExecuteCSR as CSR
import Control.Monad
import Control.Monad.Trans.Maybe

execute :: (RiscvProgram p t, MonadPlus p) => Instruction -> p ()
execute InvalidInstruction = do
  raiseException 0 2
--  cycles <- getCSRField Field.MCycle -- NOTE: should we count or not count an invalid instruction -> check later, if yes it should come before raiseException
--  setCSRField Field.MCycle (cycles + 1)
execute inst = do
  msum (map (\f -> f inst) [I.execute, I64.execute, M.execute, M64.execute, CSR.execute])
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
