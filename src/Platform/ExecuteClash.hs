module Platform.ExecuteClash where
import Spec.Decode
import Spec.Machine
import qualified Spec.CSRField as Field
import Spec.ExecuteI as I
import Spec.ExecuteM as M
import Spec.ExecuteCSR as CSR
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude

execute :: (RiscvMachine p t) => Instruction -> p ()
execute inst = do
  case inst of
    IInstruction   i     -> I.execute   i
--    MInstruction   i   -> M.execute   i
--    I64Instruction i   -> I64.execute i
--    M64Instruction i   -> M64.execute i
--    CSRInstruction i     -> CSR.execute i
    --InvalidInstruction i -> raiseExceptionWithInfo 0 2 i
    _ -> return () --raiseExceptionWithInfo 0 2 0
  --cycles <- getCSRField Field.MCycle
  --setCSRField Field.MCycle (cycles + 1)
  --instret <- getCSRField Field.MInstRet
  --setCSRField Field.MInstRet (instret + 1)
