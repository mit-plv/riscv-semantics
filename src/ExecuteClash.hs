module ExecuteClash where
import Decode
import Program
import qualified CSRField as Field
import ExecuteI as I
import ExecuteM as M
import ExecuteCSR as CSR
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude 

execute :: (RiscvProgram p t) => Instruction -> p ()
execute inst = do
  case inst of
    IInstruction   i   -> I.execute   i
--    MInstruction   i   -> M.execute   i
--    I64Instruction i   -> I64.execute i
--    M64Instruction i   -> M64.execute i
    CSRInstruction i   -> CSR.execute i
    InvalidInstruction -> raiseException 0 2
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
