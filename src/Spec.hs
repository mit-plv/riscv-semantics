module Spec where
import Program
import VirtualMemory
import Execute
import Decode
import Utility
import qualified CSRField as Field
import Data.Int
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Word

runCycle :: (RiscvProgram p t) => InstructionSet -> (Int32 -> p Bool) -> p () -> p ()
runCycle iset preDecode preCommit = do
  vpc <- getPC
  pc <- translate Instruction 4 vpc
  inst <- loadWord pc
  continue <- preDecode inst
  if continue
    then do
    setPC (vpc + 4)
    size <- getXLEN
    execute (decode iset ((fromIntegral :: Int32 -> MachineInt) inst))
    preCommit
    else
    return ()

-- Useful helper, not actually part of spec.
stepHelper :: (RiscvProgram p t) => InstructionSet -> Maybe t -> p Bool -> p t
stepHelper iset maybeToHostAddress checkInterrupt = do
  toHostValue <- case maybeToHostAddress of
    Nothing -> return 0 -- default value
    Just toHostAddress -> loadWord toHostAddress
  if toHostValue /= 0
    then do
      -- quit running
      if toHostValue == 1
        then return 0
        else return 1
    else do
    result <- runMaybeT (runCycle iset preDecode preCommit)
    case result of
      Nothing -> commit >> stepHelper iset maybeToHostAddress checkInterrupt
      Just _ -> getRegister 10
  where preDecode inst = return (inst /= 0x6f)
        preCommit = do
          interrupt <- lift checkInterrupt
          when interrupt (setCSRField Field.MEIP 1)
          endCycle
