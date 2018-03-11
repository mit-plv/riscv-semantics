module ExecuteUnsupported where
import Decode
import Program
import qualified CSRField as Field
import Control.Monad
import Control.Monad.Trans.Maybe

execute :: (RiscvProgram p t, MonadPlus p) => Instruction -> p ()
execute _ = do
  raiseException 0 2 -- Illegal instruction code
--  cycles <- getCSRField Field.MCycle --Same a InvalidInstruction, should we increase MCycle?
--  setCSRField Field.MCycle (cycles + 1)

