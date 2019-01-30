module Spec.CSR where
import Utility.Utility
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Tuple
import Prelude
import qualified Prelude as P

-- Machine-level CSRs.
data CSR = MHartID | MISA | MStatus | MTVec | MEDeleg | MIDeleg | MIP | MIE | MCycle |
           MInstRet | MCounterEn | MScratch | MEPC | MCause | MTVal |
  MHPMCounter3 | MHPMCounter4 | MHPMCounter5 | MHPMCounter6 | MHPMCounter7 | MHPMCounter8 | MHPMCounter9 | MHPMCounter10 | MHPMCounter11 | MHPMCounter12 | MHPMCounter13 | MHPMCounter14 | MHPMCounter15 | MHPMCounter16 | MHPMCounter17 | MHPMCounter18 | MHPMCounter19 | MHPMCounter20 | MHPMCounter21 | MHPMCounter22 | MHPMCounter23 | MHPMCounter24 | MHPMCounter25 | MHPMCounter26 | MHPMCounter27 | MHPMCounter28 | MHPMCounter29 | MHPMCounter30 | MHPMCounter31 |


-- Supervisor-level CSRs
           SStatus | SEDeleg | SIDeleg | STVec | SIP | SIE | SCounterEn | SScratch | SEPC |
           SCause | STVal | SATP |
     -- User-mode CSRs
           UStatus | UIE | UTVec | UScratch | UEPC | UCause | UTVal | UIP |
           FFlags | FRM | FCSR | Time | Cycle | InstRet |
           InvalidCSR
  deriving Eq

-- For Clash's sake; otherwise, this could be an enum.
lookupCSR :: (Integral x) => x -> CSR
lookupCSR 0x300 = MStatus
lookupCSR 0x301 = MISA
lookupCSR 0x302 = MEDeleg
lookupCSR 0x303 = MIDeleg
lookupCSR 0x304 = MIE
lookupCSR 0x305 = MTVec
lookupCSR 0x306 = MCounterEn
lookupCSR 0x340 = MScratch
lookupCSR 0x341 = MEPC
lookupCSR 0x342 = MCause
lookupCSR 0x343 = MTVal
lookupCSR 0x344 = MIP
lookupCSR 0xB00 = MCycle
lookupCSR 0xB02 = MInstRet
lookupCSR 0xB03 = MHPMCounter3
lookupCSR 0xB04 = MHPMCounter4
lookupCSR 0xB05 = MHPMCounter5
lookupCSR 0xB06 = MHPMCounter6
lookupCSR 0xB07 = MHPMCounter7
lookupCSR 0xB08 = MHPMCounter8
lookupCSR 0xB09 = MHPMCounter9
lookupCSR 0xB0A = MHPMCounter10
lookupCSR 0xB0B = MHPMCounter11
lookupCSR 0xB0C = MHPMCounter12
lookupCSR 0xB0D = MHPMCounter13
lookupCSR 0xB0E = MHPMCounter14
lookupCSR 0xB0F = MHPMCounter15
lookupCSR 0xB10 = MHPMCounter16
lookupCSR 0xB11 = MHPMCounter17
lookupCSR 0xB12 = MHPMCounter18
lookupCSR 0xB13 = MHPMCounter19
lookupCSR 0xB14 = MHPMCounter20
lookupCSR 0xB15 = MHPMCounter21
lookupCSR 0xB16 = MHPMCounter22
lookupCSR 0xB17 = MHPMCounter23
lookupCSR 0xB18 = MHPMCounter24
lookupCSR 0xB19 = MHPMCounter25
lookupCSR 0xB1A = MHPMCounter26
lookupCSR 0xB1B = MHPMCounter27
lookupCSR 0xB1C = MHPMCounter28
lookupCSR 0xB1D = MHPMCounter29
lookupCSR 0xB1E = MHPMCounter30
lookupCSR 0xB1F = MHPMCounter31
lookupCSR 0x100 = SStatus
lookupCSR 0x102 = SEDeleg
lookupCSR 0x103 = SIDeleg
lookupCSR 0x104 = SIE
lookupCSR 0x105 = STVec
lookupCSR 0x106 = SCounterEn
lookupCSR 0x140 = SScratch
lookupCSR 0x141 = SEPC
lookupCSR 0x142 = SCause
lookupCSR 0x143 = STVal
lookupCSR 0x144 = SIP
lookupCSR 0x180 = SATP
lookupCSR 0x000 = UStatus
lookupCSR 0x001 = FFlags
lookupCSR 0x002 = FRM
lookupCSR 0x003 = FCSR
lookupCSR 0x004 = UIE
lookupCSR 0x005 = UTVec
lookupCSR 0x040 = UScratch
lookupCSR 0x041 = UEPC
lookupCSR 0x042 = UCause
lookupCSR 0x043 = UTVal
lookupCSR 0x044 = UIP
lookupCSR 0xC00 = Cycle
lookupCSR 0xC01 = Time
lookupCSR 0xC02 = InstRet
lookupCSR 0xF14 = MHartID
lookupCSR _ = InvalidCSR
