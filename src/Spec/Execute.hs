module Spec.Execute where
import Spec.Decode
import Spec.Machine
import qualified Spec.CSRField as Field
import Spec.ExecuteI as I
import Spec.ExecuteI64 as I64
import Spec.ExecuteM as M
import Spec.ExecuteM64 as M64
import Spec.ExecuteA as A
import Spec.ExecuteA64 as A64
import Spec.ExecuteF as F
import Spec.ExecuteF64 as F64
import Spec.ExecuteCSR as CSR


-- Note: instructions belonging to unsupported extensions were already filtered out by the decoder
execute :: (RiscvMachine p t) => Instruction -> p ()
execute inst = do
  case inst of
    IInstruction   i     -> I.execute   i
    MInstruction   i     -> M.execute   i
    AInstruction   i     -> A.execute   i
    FInstruction   i     -> F.execute   i
    I64Instruction i     -> I64.execute i
    M64Instruction i     -> M64.execute i
    A64Instruction i     -> A64.execute i
    F64Instruction i     -> F64.execute i
    CSRInstruction i     -> CSR.execute i
    InvalidInstruction i -> raiseExceptionWithInfo 0 2 i
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
