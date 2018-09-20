{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteF64 where
import Decode
import Program

execute :: forall p t. (RiscvProgram p t) => InstructionF64 -> p ()
execute inst = error $ "dispatch bug: " ++ show inst
