module CSRSpec where
import CSR
import Program
import Utility
import qualified CSRField as Field
import Data.Bits

getCSR :: (RiscvProgram p t u) => CSR -> p MachineInt

getCSR MStatus = do
  mpie <- getCSRField Field.MPIE
  mie <- getCSRField Field.MIE
  return (shift mpie 7 .|. shift mie 3)

getCSR MIP = do
  meip <- getCSRField Field.MEIP
  mtip <- getCSRField Field.MTIP
  return (shift meip 11 .|. shift mtip 7)

getCSR MIE = do
  meie <- getCSRField Field.MEIE
  mtie <- getCSRField Field.MTIE
  return (shift meie 11 .|. shift mtie 7)

getCSR MTVec = do
  base <- getCSRField Field.MTVecBase
  mode <- getCSRField Field.MTVecMode
  return (shift base 2 .|. mode)

-- Read-only
getCSR MCycle = getCSRField Field.MCycle

-- Read-only
getCSR MInstRet = getCSRField Field.MInstRet

getCSR MEPC = getCSRField Field.MEPC

getCSR MScratch = getCSRField Field.MScratch

getCSR MCause = do
  xlen <- getXLEN
  code <- getCSRField Field.MCauseCode
  interrupt <- getCSRField Field.MCauseInterrupt
  return (shift interrupt (xlen - 1) .|. code)

-- Catch-all for other (possibly unimplemented) CSRs; hardwire to 0.
getCSR _ = return 0

setCSR :: (RiscvProgram p t u, Integral x, Bits x) => CSR -> x -> p ()

setCSR MStatus val = do
  setCSRField Field.MIE (bitSlice val 3 4)
  setCSRField Field.MPIE (bitSlice val 7 8)

setCSR MIP val = do
  setCSRField Field.MTIP (bitSlice val 7 8)
  setCSRField Field.MEIP (bitSlice val 11 12)

setCSR MIE val = do
  setCSRField Field.MTIE (bitSlice val 7 8)
  setCSRField Field.MEIE (bitSlice val 11 12)

setCSR MTVec val = do
  setCSRField Field.MTVecMode (bitSlice val 0 2)
  setCSRField Field.MTVecBase (shift val (-2))

setCSR MEPC val = setCSRField Field.MEPC val

setCSR MScratch val = setCSRField Field.MScratch val

setCSR MCause val = do
  xlen <- getXLEN
  setCSRField Field.MCauseCode (bitSlice val 0 (xlen - 1))
  setCSRField Field.MCauseInterrupt (bitSlice val (xlen - 1) xlen)

setCSR _ _ = return ()
