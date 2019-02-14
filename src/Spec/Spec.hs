module Spec.Spec where
import Spec.Machine
import Spec.VirtualMemory
import Spec.Execute
import Spec.Decode
import Utility.Utility
import qualified Spec.CSRField as Field
import Data.Int
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Word
import qualified Platform.Plic as Plic

-- TODO: Cleanup/rename/move this file.
runCycle :: (RiscvMachine p t) => InstructionSet -> (Int32 -> p Bool) -> p () -> p ()
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
    return $! ()

-- Useful helper, not actually part of spec.
stepHelper :: (RiscvMachine p t) => InstructionSet -> Maybe t -> p Plic.ChangeMIP -> p (t,t) -> p t
stepHelper iset maybeToHostAddress checkExternalInterrupt mtimecmpAndMtime = do
  toHostValue <- case maybeToHostAddress of
    Nothing -> return $! 0 -- default value
    Just toHostAddress -> loadWord toHostAddress
  if toHostValue /= 0
    then do
      -- quit running
      if toHostValue == 1
        then return $! 0
        else return $! 1
    else do
    checkInterrupt
    result <- runMaybeT (runCycle iset preDecode preCommit)
    case result of
      Nothing -> commit >>  stepHelper iset maybeToHostAddress checkExternalInterrupt mtimecmpAndMtime
      Just _ -> getRegister 10
  where preDecode inst = return (inst /= 0x6f)
        preCommit = do
          interrupt <- lift checkExternalInterrupt
          when (interrupt == Plic.Set) (setCSRField Field.MEIP 1)
          when (interrupt == Plic.Reset) (setCSRField Field.MEIP 0)
          endCycle
        checkInterrupt = do
             (mtime, mtimecmp) <- mtimecmpAndMtime
             setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
             -- Check for interrupts before updating PC.
             mie <- getCSRField Field.MIE
             meie <- getCSRField Field.MEIE
             meip <- getCSRField Field.MEIP
             mtie <- getCSRField Field.MTIE
             mtip <- getCSRField Field.MTIP
             nPC <- getPC
             k <- if (mie > 0 && ((meie > 0 && meip > 0) || (mtie > 0 && mtip > 0))) then do
                       -- Disable interrupts
                       setCSRField Field.MIE 0
                       interruptHappen <- if (meie > 0 && meip > 0) then do
                               -- Remove pending external interrupt
                               setCSRField Field.MEIP 0
                               runMaybeT (raiseException 1 11)-- Machine external interrupt.
                               return $! True
                             else if (mtie > 0 && mtip > 0) then do
                               runMaybeT (raiseException 1 7) -- Machine timer interrupt.
                               return $! True
                             else return $! False
                       -- Save the PC of the next (unexecuted) instruction.
                       setCSRField Field.MEPC nPC
                       return $! interruptHappen
                  else return $! False
             if (k) then
                  commit
             else
                  return ()

