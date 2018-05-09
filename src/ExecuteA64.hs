{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteA64 where
import Decode
import Program
import Utility
import Control.Monad
import Prelude

-- TODO!

execute :: forall p t. (RiscvProgram p t) => InstructionA64 -> p ()
-- begin ast
execute (Lr_d rd rs1 aqrl) = raiseException 0 2
execute (Sc_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoswap_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoadd_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoand_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoor_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoxor_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomax_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomaxu_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomin_d rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amominu_d rd rs1 rs2 aqrl) = raiseException 0 2
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
