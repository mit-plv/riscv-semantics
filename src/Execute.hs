module Execute where
import Decode
import Program
import ExecuteI as I
import ExecuteI64 as I64
import ExecuteM as M
import ExecuteM64 as M64
import ExecuteCSR as CSR
import Control.Monad

execute :: (RiscvProgram p t u) => Instruction -> p ()
execute inst = msum (map (\f -> f inst) [I.execute, I64.execute, M.execute, M64.execute, CSR.execute])
