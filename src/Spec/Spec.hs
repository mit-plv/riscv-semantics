module Spec.Spec where
import Spec.Machine
import Spec.VirtualMemory
import Spec.Execute
import Spec.Decode
import Spec.CSRSpec
import Spec.CSR
import Utility.Utility
import qualified Spec.CSRField as Field
import Data.Int
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Word
import qualified Platform.Plic as Plic
import Debug.Trace

-- TODO: Cleanup/rename/move this file.
runCycle :: (RiscvMachine p t) => InstructionSet -> (Int32 -> p Bool) -> p () -> p ()
runCycle iset preDecode preCommit = do
  vpc <- getPC
  pc <- translate Instruction 4 vpc
  inst <- loadWord Fetch pc
  continue <- preDecode inst
  if continue
    then do
    setPC (vpc + 4)
    execute (decode iset ((fromIntegral :: Int32 -> MachineInt) inst))
    preCommit 
    else
    return $! ()

-- Useful helper, not actually part of spec.
stepHelper :: (RiscvMachine p t) => InstructionSet -> Maybe t -> (MaybeT p) Plic.ChangeMIP -> (MaybeT p) (t,t) -> (Int32 -> (MaybeT p) Bool ) -> (p) () -> p t
stepHelper iset maybeToHostAddress checkExternalInterrupt mtimecmpAndMtime preDecode preCommit = do
  toHostValue <- case maybeToHostAddress of
    Nothing -> return $! 0 -- default value
    Just toHostAddress -> loadWord Execute toHostAddress
  if toHostValue /= 0
    then do
      -- quit running
      if toHostValue == 1
        then return $! 0
        else return $! 1
    else do
--    checkInterrupt
    result <- runMaybeT (checkInterrupt >> (runCycle iset preDecode endCycle))
    case result of
      Nothing -> preCommit >> commit >>  stepHelper iset maybeToHostAddress checkExternalInterrupt mtimecmpAndMtime preDecode preCommit
      Just _ -> getRegister 10
  where
    checkInterrupt = do
             interrupt <- checkExternalInterrupt
             when (interrupt == Plic.Set) $! (do
                             
                          trace "Set meip" $ setCSRField Field.MEIP 1)
             when (interrupt == Plic.Reset) $! (do --SHOULD NOT BE POSSIBLE
                          trace "Reset meip" $ setCSRField Field.MEIP 0)

             (mtimecmp, mtime) <- mtimecmpAndMtime
             setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
             --setCSRField Field.STIP (fromEnum (mtime >= mtimecmp))
                          
             --when (mtime >= mtimecmp) . return $! ()
             -- Check for interrupts before updating PC.
             miefield <- getCSRField Field.MIE
             meie <- getCSRField Field.MEIE
             meip <- getCSRField Field.MEIP
             mtie <- getCSRField Field.MTIE
             mtip <- getCSRField Field.MTIP
             nPC <- getPC
             priv <- getPrivMode
             mip <- fmap (fromIntegral :: MachineInt -> Int32)$ getCSR MIP
             mie <- fmap (fromIntegral :: MachineInt -> Int32)$ getCSR MIE
             siefield <- getCSRField Field.SIE
             let readyInterrupts = mip .&. mie
             mideleg <- fmap (fromIntegral :: MachineInt -> Int32) $ getCSR MIDeleg 
             let readyMachineInterrupts = readyInterrupts .&. (complement mideleg)
             let machineInterruptsEnabled = (miefield == 1) || (priv < Machine)
             let readySupervisorInterrupts = readyInterrupts .&. mideleg
             let supervisorInterruptsEnabled = ((siefield == 1) && (priv == Supervisor)) || (priv < Supervisor);
             let realReadyInterrupts = (if machineInterruptsEnabled then readyMachineInterrupts else 0) .|. (if supervisorInterruptsEnabled then readySupervisorInterrupts else  0) 
             k <- if (realReadyInterrupts /=0)
                              then do
                       -- Disable interrupts
                      -- setCSRField Field.MIE 0
                       let ssip = bitSlice realReadyInterrupts 1 2
                       let msip = bitSlice realReadyInterrupts 3 4
                       let stip = bitSlice realReadyInterrupts 5 6
                       let mtip = bitSlice realReadyInterrupts 7 8
                       let seip = bitSlice realReadyInterrupts 9 10
                       let meip = bitSlice realReadyInterrupts 11 12
                       interruptHappen <- if (seip == 1) then do
                               -- Remove pending external interrupt
                               setCSRField Field.SEIP 0
--                               runMaybeT (raiseException 1 9)-- Machine external interrupt.
                               trace "Raise external supervisor interrupt!" $ raiseException 1 9-- Machine external interrupt.
                               return $! True
                             else if (meip == 1) then do
                               setCSRField Field.MEIP 0
                               --runMaybeT (raiseException 1 11) -- Machine timer interrupt.
                               trace "Raise machine external interrupt " $ raiseException 1 11 -- Machine timer interrupt.
                               return $! True
                             else if (stip == 1) then do
                               setCSRField Field.STIP 0
                               raiseException 1 5 -- Machine timer interrupt.
                               return $! True
                               else if (mtip == 1) then do
                                    setCSRField Field.MTIP 0
                                    raiseException 1 7 -- Machine timer interrupt.
                                    return $! True
                                    else 
                                    return $! False --Impossible path
                       -- Save the PC of the next (unexecuted) instruction.
--                       setCSRField Field.MEPC nPC
                       return $! interruptHappen
                  else return $! False
             if (k) then do
                  endCycle 
             else do
--                  (mtimecmp, mtime) <- mtimecmpAndMtime
--                  setCSRField Field.MTIP (fromEnum (mtime >= mtimecmp))
--                  setCSRField Field.STIP (fromEnum (mtime >= mtimecmp))
                  return $! ()

