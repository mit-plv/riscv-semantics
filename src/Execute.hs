module Execute where
import Decode
import Program
import qualified CSRField as Field
import ExecuteI as I
import ExecuteI64 as I64
import ExecuteM as M
import ExecuteM64 as M64
import ExecuteCSR as CSR


-- Note: instructions belonging to unsupported extensions were already filtered out by the decoder
execute :: (RiscvProgram p t) => Instruction -> p ()
execute inst = do
  case inst of
    IInstruction   i     -> I.execute   i
    MInstruction   i     -> M.execute   i
    AInstruction   i     -> raiseExceptionWithInfo 0 2 i -- TODO
    I64Instruction i     -> I64.execute i
    M64Instruction i     -> M64.execute i
    A64Instruction i     -> raiseExceptionWithInfo 0 2 i -- TODO
    CSRInstruction i     -> CSR.execute i
    InvalidInstruction i -> raiseExceptionWithInfo 0 2 i
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)
