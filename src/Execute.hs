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

executeInvalid :: (RiscvProgram p t) => Instruction -> p ()
executeInvalid InvalidInstruction = do
  raiseException 0 2
--  cycles <- getCSRField Field.MCycle -- NOTE: should we count or not count an invalid instruction -> check later, if yes it should come before raiseException
--  setCSRField Field.MCycle (cycles + 1)

executeValid :: (RiscvProgram p t) => (Instruction -> p ()) -> Instruction -> p ()
executeValid f inst = do
  f inst
  cycles <- getCSRField Field.MCycle
  setCSRField Field.MCycle (cycles + 1)
  instret <- getCSRField Field.MInstRet
  setCSRField Field.MInstRet (instret + 1)

-- Note: instructions belonging to unsupported extensions were already filtered out by the decoder
execute :: (RiscvProgram p t) => Instruction -> p ()
execute = applyByExtInstructionMapper $ ByExtInstructionMapper {
  map_Invalid = executeInvalid,
  map_I = executeValid I.execute,
  map_M = executeValid M.execute,
  map_I64 = executeValid I64.execute,
  map_M64 = executeValid M64.execute,
  map_CSR = executeValid CSR.execute
}
