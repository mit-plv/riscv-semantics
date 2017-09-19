module Execute where
import Decode
import Program
import ExecuteI as I
import ExecuteM as M
import ExecuteCSR as CSR
import Control.Monad

execute :: (RiscvProgram p t u) => Instruction -> p ()
execute inst = msum (map (\f -> f inst) [I.execute, M.execute, CSR.execute])
