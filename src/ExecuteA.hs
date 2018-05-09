{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteA where
import Decode
import Program
import Utility
import Control.Monad
import Prelude

-- TODO!

execute :: forall p t. (RiscvProgram p t) => InstructionA -> p ()
-- begin ast
execute (Lr_w rd rs1 aqrl) = raiseException 0 2
execute (Sc_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoswap_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoadd_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoand_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoor_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amoxor_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomax_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomaxu_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amomin_w rd rs1 rs2 aqrl) = raiseException 0 2
execute (Amominu_w rd rs1 rs2 aqrl) = raiseException 0 2
-- end ast
execute inst = error $ "dispatch bug: " ++ show inst
