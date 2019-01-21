module CSRSpec where
import CSR
import Program
import Utility
import qualified CSRField as Field
import Data.Bits
import Control.Monad
import Prelude

-- Permission checks are handled in ExecuteCSR.

getCSR :: (RiscvProgram p t) => CSR -> p MachineInt

-- Currently ignores writes.
getCSR MISA = do
  xlen <- getXLEN
  mxl <- getCSRField Field.MXL
  ext <- getCSRField Field.Extensions
  return (shift mxl (xlen - 2) .|. ext)

getCSR MHartID = do
  return 0
getCSR MStatus = do
  tsr <- getCSRField Field.TSR
  tw <- getCSRField Field.TW
  tvm <- getCSRField Field.TVM
  mprv <- getCSRField Field.MPRV
  mpp <- getCSRField Field.MPP
  mpie <- getCSRField Field.MPIE
  mie <- getCSRField Field.MIE
  sstatus <- getCSR SStatus
  return (shift tsr 22 .|. shift tw 21 .|. shift tvm 20 .|. shift mprv 17 .|.
          shift mpp 11 .|. shift mpie 7 .|. shift mie 3 .|. sstatus)

getCSR MEDeleg = getCSRField Field.MEDeleg

getCSR MIDeleg = getCSRField Field.MIDeleg

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

getCSR InstRet = do
  permS <- getCSRField Field.MIR
  permU <- getCSRField Field.SIR
  priv <- getPrivMode
-- TODO here we hardcode that user mode is supported.
  if (priv == Machine ||
      (priv == Supervisor && permS == 1) ||
      (priv == User && permS == 1 && permU == 1))
    then
    getCSRField Field.MInstRet
    else
    raiseException 0 2

getCSR Time = do
  permS <- getCSRField Field.MTM
  permU <-  getCSRField Field.STM
  priv <- getPrivMode
-- TODO here we hardcode that user mode is supported.
  if (priv == Machine ||
      (priv == Supervisor && permS == 1) ||
      (priv == User && permS == 1 && permU == 1))
    then do
    timer <- loadWord 0x200bff8 --Hardcode for Minimal. TODO from platform. BUG it should be DOUBLE word not word. For size 64.
    return (fromIntegral timer)
 -- getCSRField timer -- TODO FIX BUG here
    else
    raiseException 0 2


getCSR Cycle = do
  permS <- getCSRField Field.MCY
  permU <-  getCSRField Field.SCY
  priv <- getPrivMode
-- TODO here we hardcode that user mode is supported.
  if (priv == Machine ||
      (priv == Supervisor && permS == 1) ||
      (priv == User && permS == 1 && permU == 1))
    then
    getCSRField Field.MCycle -- TODO FIX BUG here
    else
    raiseException 0 2

getCSR MHPMCounter3 = undefined
getCSR MHPMCounter4 = undefined
getCSR MHPMCounter5 = undefined
getCSR MHPMCounter6 = undefined
getCSR MHPMCounter7 = undefined
getCSR MHPMCounter8 = undefined
getCSR MHPMCounter9 = undefined
getCSR MHPMCounter10 = undefined
getCSR MHPMCounter11 = undefined
getCSR MHPMCounter12 = undefined
getCSR MHPMCounter13 = undefined
getCSR MHPMCounter14 = undefined
getCSR MHPMCounter15 = undefined
getCSR MHPMCounter16 = undefined
getCSR MHPMCounter17 = undefined
getCSR MHPMCounter18 = undefined
getCSR MHPMCounter19 = undefined
getCSR MHPMCounter20 = undefined
getCSR MHPMCounter21 = undefined
getCSR MHPMCounter22 = undefined
getCSR MHPMCounter23 = undefined
getCSR MHPMCounter24 = undefined
getCSR MHPMCounter25 = undefined
getCSR MHPMCounter26 = undefined
getCSR MHPMCounter27 = undefined
getCSR MHPMCounter28 = undefined
getCSR MHPMCounter29 = undefined
getCSR MHPMCounter30 = undefined
getCSR MHPMCounter31 = undefined

                --MHPM | MIR | MTM | MCY | -- mcounteren

getCSR MCounterEn = do
  mhpm <- getCSRField Field.MHPM
  mir <- getCSRField Field.MIR
  mtm <- getCSRField Field.MTM
  mcy <- getCSRField Field.MCY
  return (mcy .|. shift mtm 1 .|. shift mir 2 .|. shift mhpm 3)

getCSR MCause = do
  xlen <- getXLEN
  code <- getCSRField Field.MCauseCode
  interrupt <- getCSRField Field.MCauseInterrupt
  return (shift interrupt (xlen - 1) .|. code)

getCSR MTVal = getCSRField Field.MTVal

-- Supervisor-level CSRs:

getCSR SStatus = do
  mxr <- getCSRField Field.MXR
  sum <- getCSRField Field.SUM
  spp <- getCSRField Field.SPP
  spie <- getCSRField Field.SPIE
  sie <- getCSRField Field.SIE
  let base = (shift mxr 19 .|. shift sum 18 .|. shift spp 8 .|.
              shift spie 5 .|. shift sie 1)
  xlen <- getXLEN
  if xlen > 32
    then do
    -- SXL and UXL are currently hardwired to MXL.
    mxl <- getCSRField Field.MXL
    return (shift mxl 32 .|. shift mxl 34 .|. base)
    else return base

getCSR STVec = do
  base <- getCSRField Field.STVecBase
  mode <- getCSRField Field.STVecMode
  return (shift base 2 .|. mode)

getCSR SEPC = getCSRField Field.SEPC

getCSR SCounterEn = do
  shpm <- getCSRField Field.SHPM
  sir <- getCSRField Field.SIR
  stm <- getCSRField Field.STM
  scy <- getCSRField Field.SCY
  return (scy .|. shift stm 1 .|. shift sir 2 .|. shift shpm 3)


getCSR SScratch = getCSRField Field.SScratch

getCSR SCause = do
  xlen <- getXLEN
  code <- getCSRField Field.SCauseCode
  interrupt <- getCSRField Field.SCauseInterrupt
  return (shift interrupt (xlen - 1) .|. code)

getCSR STVal = getCSRField Field.STVal

getCSR SATP = do
  tvm <- getCSRField Field.TVM
  when (tvm == 1) (raiseException 0 2)
  xlen <- getXLEN
  mode <- getCSRField Field.MODE
  asid <- getCSRField Field.ASID
  ppn <- getCSRField Field.PPN
  if xlen == 32 then do
    return (shift mode 31 .|. shift asid 22 .|. ppn)
    else do
    return (shift mode 60 .|. shift asid 44 .|. ppn)

-- User-level CSRs:

getCSR FFlags = getCSRField Field.FFlags

getCSR FRM = getCSRField Field.FRM

getCSR FCSR = do
  fflags <- getCSRField Field.FFlags
  frm <- getCSRField Field.FRM
  return (shift frm 5 .|. fflags)

-- It's unclear whether these are allowed to raise an exception in machine mode.
-- It's problematic if Time isn't, since that brings some extra platform
-- dependent information (specifically, the MTime memory-mapped address) up into
-- the CSR specification.
-- getCSR InstRet = raiseException 0 2
-- getCSR Cycle = raiseException 0 2
-- getCSR Time = raiseException 0 2

-- Catch-all for other (possibly unimplemented) CSRs; hardwire to 0.
getCSR _ = raiseException 0 2

setCSR :: (RiscvProgram p t, Integral x, Bits x) => CSR -> x -> p ()

setCSR MStatus val = do
  setCSR SStatus val
  setCSRField Field.TSR (bitSlice val 22 23)
  setCSRField Field.TW (bitSlice val 21 22)
  setCSRField Field.TVM (bitSlice val 20 21)
  setCSRField Field.MPRV (bitSlice val 17 18)
  setCSRField Field.MPP (bitSlice val 11 13)
  setCSRField Field.MPIE (bitSlice val 7 8)
  setCSRField Field.MIE (bitSlice val 3 4)

setCSR MEDeleg val = setCSRField Field.MEDeleg (val .&. complement (shift 1 11))

setCSR MIDeleg val = setCSRField Field.MIDeleg val

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

setCSR MCounterEn val = do
  let mhpm = shift val (-3)
  let mir = bitSlice val 2 3
  let mtm = bitSlice val 1 2
  let mcy = bitSlice val 0 1
  setCSRField Field.MHPM mhpm
  setCSRField Field.MIR mir
  setCSRField Field.MTM mtm
  setCSRField Field.MCY mcy

setCSR MScratch val = setCSRField Field.MScratch val

setCSR MCause val = do
  xlen <- getXLEN
  setCSRField Field.MCauseCode (bitSlice val 0 (xlen - 1))
  setCSRField Field.MCauseInterrupt (bitSlice val (xlen - 1) xlen)

setCSR MTVal val = setCSRField Field.MTVal val

-- Supervisor-level CSRs:

setCSR SStatus val = do
  setCSRField Field.MXR (bitSlice val 19 20)
  setCSRField Field.SUM (bitSlice val 18 19)
  setCSRField Field.SPP (bitSlice val 8 9)
  setCSRField Field.SPIE (bitSlice val 5 6)
  setCSRField Field.SIE (bitSlice val 1 2)

setCSR STVec val = do
  setCSRField Field.STVecMode (bitSlice val 0 2)
  setCSRField Field.STVecBase (shift val (-2))

setCSR SEPC val = setCSRField Field.SEPC val

setCSR SScratch val = setCSRField Field.SScratch val

setCSR SCounterEn val = do
  let shpm = shift val (-3)
  let sir = bitSlice val 2 3
  let stm = bitSlice val 1 2
  let scy = bitSlice val 0 1
  setCSRField Field.SHPM shpm
  setCSRField Field.SIR sir
  setCSRField Field.STM stm
  setCSRField Field.SCY scy

setCSR SCause val = do
  xlen <- getXLEN
  setCSRField Field.SCauseCode (bitSlice val 0 (xlen - 1))
  setCSRField Field.SCauseInterrupt (bitSlice val (xlen - 1) xlen)

setCSR STVal val = setCSRField Field.STVal val

setCSR SATP val = do
  priv <- getPrivMode
  tvm <- getCSRField Field.TVM
  when (priv == Supervisor && tvm == 1) (raiseException 0 2)
  xlen <- getXLEN
  let mode = if xlen == 32 then bitSlice val 31 32 else bitSlice val 60 64
  -- If the mode is unsupported, the write has no effect.
  when (mode `elem` [1, 8, 9]) $ do
    if xlen == 32
      then do
      setCSRField Field.MODE mode
      setCSRField Field.ASID (bitSlice val 22 31)
      setCSRField Field.PPN (bitSlice val 0 22)
      else do
      setCSRField Field.MODE mode
      setCSRField Field.ASID (bitSlice val 44 60)
      setCSRField Field.PPN (bitSlice val 0 44)

-- User-level CSRs:

setCSR FFlags val = setCSRField Field.FFlags (bitSlice val 0 5)

setCSR FRM val = setCSRField Field.FRM (bitSlice val 0 3)

setCSR FCSR val = do
  setCSRField Field.FFlags (bitSlice val 0 5)
  setCSRField Field.FRM (bitSlice val 5 8)

setCSR _ _ = raiseException 0 2


