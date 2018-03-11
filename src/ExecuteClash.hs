module ExecuteClash where
import Decode
import Program
import qualified CSRField as Field
import ExecuteI as I
import ExecuteM as M
import ExecuteCSR as CSR
import ExecuteUnsupported as Unsupported
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude 
execute :: (RiscvProgram p t, MonadPlus p ) => Instruction -> p ()
execute InvalidInstruction = do
  raiseException 0 2
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
execute inst = do
  (msum (map (\f -> f inst) [I.execute, CSR.execute, Unsupported.execute]))
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
