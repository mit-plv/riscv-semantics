module Spec.Execute32 where
import Spec.Decode
import Spec.Machine
import qualified Spec.CSRField as Field
import Spec.ExecuteI as I
import Spec.ExecuteM as M
import Spec.ExecuteCSR as CSR
import Control.Monad
import Control.Monad.Trans.Maybe
-- Deprecated
--execute :: (RiscvMachine p t) => Instruction -> p ()
--execute (InvalidInstruction _) = do
--  raiseException 0 2
--  cycles <- getCSRField Field.MCycle
--  setCSRField Field.MCycle (cycles + 1)
--execute inst = do
--  _ <- runMaybeT (msum (map (\f -> f inst) [I.execute, M.execute, CSR.execute]))
--  cycles <- getCSRField Field.MCycle
--  setCSRField Field.MCycle (cycles + 1)
--  instret <- getCSRField Field.MInstRet
--  setCSRField Field.MInstRet (instret + 1)
--