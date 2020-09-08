module Spec.CSRGetSet where
import Spec.CSR
import Spec.Machine
import Utility.Utility
import qualified Spec.CSRField as Field
import Data.Bits
import Control.Monad
import Prelude
import Debug.Trace
-- Permission checks are handled in ExecuteCSR.

-- helper function to make it very clear to Coq that getCSR terminates
getCSR_SStatus :: (RiscvMachine p t) => p MachineInt
getCSR_SStatus = do
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
    -- return (shift mxl 32 .|. shift mxl 34 .|. base)
    vpc <- getPC
    --return $ (\x-> trace ((show $ (fromIntegral vpc)) ++ " " ++ show x) x)(shift mxl 32 .|. base)
    return $ (shift mxl 32 .|. base)
    else return base


getCSR :: (RiscvMachine p t) => CSR -> p MachineInt

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
  -- mxl = sxl = uxl for now
  mxl <- getCSRField Field.MXL
  sstatus <- getCSR_SStatus
  return (shift tsr 22 .|. shift tw 21 .|. shift tvm 20 .|. shift mprv 17 .|.
          shift mpp 11 .|. shift mpie 7 .|. shift mie 3 .|. sstatus .|. shift mxl 34)

getCSR MEDeleg = getCSRField Field.MEDeleg

getCSR MIDeleg = getCSRField Field.MIDeleg

getCSR SIP = do
 seip <- getCSRField Field.SEIP
 ueip <- getCSRField Field.UEIP
 stip <- getCSRField Field.STIP
 utip <- getCSRField Field.UTIP
 ssip <- getCSRField Field.SSIP
 usip <- getCSRField Field.USIP
 return (usip .|. shift ssip 1 .|. --shift msie 3 .|.
         shift utip 4 .|. shift stip 5 .|. -- shift mtie 7 .|.
         shift ueip 8 .|. shift seip 9 ) -- .|.  shift meie 11 )


-- For now we don't support user level interrupts in this spec.
getCSR SIE = do
-- meie <- getCSRField Field.MEIE
 seie <- getCSRField Field.SEIE
 ueie <- return 0 -- getCSRField Field.UEIE
-- mtie <- getCSRField Field.MTIE
 stie <- getCSRField Field.STIE
 utie <- return 0 -- getCSRField Field.UTIE
--  msie <- getCSRField Field.MSIE
 ssie <- getCSRField Field.SSIE
 usie <- return 0 -- getCSRField Field.USIE
 return (usie .|. shift ssie 1 .|. --shift msie 3 .|.
         shift utie 4 .|. shift stie 5 .|. -- shift mtie 7 .|.
         shift ueie 8 .|. shift seie 9 ) -- .|.  shift meie 11 )


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

getCSR InstRet = getCSR_InstRet

getCSR Time = getCSR_Time

getCSR Cycle = getCSR_Cycle

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

getCSR SStatus = getCSR_SStatus

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

getCSR MIP = do
 meip <- getCSRField Field.MEIP
 seip <- getCSRField Field.SEIP
 ueip <- getCSRField Field.UEIP
 mtip <- getCSRField Field.MTIP
 stip <- getCSRField Field.STIP
 utip <- getCSRField Field.UTIP
 msip <- getCSRField Field.MSIP
 ssip <- getCSRField Field.SSIP
 usip <- getCSRField Field.USIP
 return (usip .|. shift ssip 1 .|. shift msip 3 .|.
         shift utip 4 .|. shift stip 5 .|. shift mtip 7 .|.
         shift ueip 8 .|. shift seip 9 .|. shift meip 11 )


getCSR MIE = do
 meie <- getCSRField Field.MEIE
 seie <- getCSRField Field.SEIE
 ueie <- return 0 -- getCSRField Field.UEIE
 mtie <- getCSRField Field.MTIE
 stie <- getCSRField Field.STIE
 utie <- return 0 -- getCSRField Field.UTIE
 msie <- getCSRField Field.MSIE
 ssie <- getCSRField Field.SSIE
 usie <- return 0 -- getCSRField Field.USIE
 return (usie .|. shift ssie 1 .|. shift msie 3 .|.
         shift utie 4 .|. shift stie 5 .|. shift mtie 7 .|.
         shift ueie 8 .|. shift seie 9 .|. shift meie 11 )

getCSR FCSR = do
  fflags <- getCSRField Field.FFlags
  frm <- getCSRField Field.FRM
  return (shift frm 5 .|. fflags)


getCSR _ = return (-1) -- Hmmm, why did I hardwire that to -1?

setCSR :: (RiscvMachine p t) => CSR -> MachineInt -> p ()

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


setCSR SIP val = do
   let usip = bitSlice val 0 1
   let ssip = bitSlice val 1 2
   let utip = bitSlice val 4 5
   let stip = bitSlice val 5 6
   let ueip = bitSlice val 8 9
   let seip = bitSlice val 9 10
   setCSRField Field.USIP usip
   setCSRField Field.SSIP ssip
   setCSRField Field.UTIP utip
   setCSRField Field.STIP stip
   setCSRField Field.UEIP ueip
   setCSRField Field.SEIP seip


setCSR SIE val = do
   let usie = bitSlice val 0 1
   let ssie = bitSlice val 1 2
   let utie = bitSlice val 4 5
   let stie = bitSlice val 5 6
   let ueie = bitSlice val 8 9
   let seie = bitSlice val 9 10
   --setCSRField Field.USIE usie
   setCSRField Field.SSIE ssie
   --setCSRField Field.UTIE utie
   setCSRField Field.STIE stie
   --setCSRField Field.UEIE ueie
   setCSRField Field.SEIE seie

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
  getCSR SStatus
  return ()

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


setCSR MIP val = do
   let usip = bitSlice val 0 1
   let ssip = bitSlice val 1 2
   let msip = bitSlice val 3 4
   let utip = bitSlice val 4 5
   let stip = bitSlice val 5 6
   let mtip = bitSlice val 7 8
   let ueip = bitSlice val 8 9
   let seip = bitSlice val 9 10
   let meip = bitSlice val 11 12
   setCSRField Field.USIP usip
   setCSRField Field.SSIP ssip
   setCSRField Field.MSIP msip
   setCSRField Field.UTIP utip
   setCSRField Field.STIP stip
   setCSRField Field.MTIP mtip
   setCSRField Field.UEIP ueip
   setCSRField Field.SEIP seip
   setCSRField Field.MEIP meip



setCSR MIE val = do
   let usie = bitSlice val 0 1
   let ssie = bitSlice val 1 2
   let msie = bitSlice val 3 4
   let utie = bitSlice val 4 5
   let stie = bitSlice val 5 6
   let mtie = bitSlice val 7 8
   let ueie = bitSlice val 8 9
   let seie = bitSlice val 9 10
   let meie = bitSlice val 11 12
  -- setCSRField Field.USIE usie
   setCSRField Field.SSIE ssie
   setCSRField Field.MSIE msie
  --setCSRField Field.UTIE utie
   setCSRField Field.STIE stie
   setCSRField Field.MTIE mtie
  -- setCSRField Field.UEIE ueie
   setCSRField Field.SEIE seie
   setCSRField Field.MEIE meie

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

setCSR _ _ = return () -- raiseException 0 2
