{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteF where
import Decode
import Program

execute :: forall p t. (RiscvProgram p t) => InstructionF -> p ()
execute inst = error $ "dispatch bug: " ++ show inst
