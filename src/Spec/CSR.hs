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
lookupCSR :: MachineInt -> CSR
lookupCSR x
  | x == 0x300 = MStatus
  | x == 0x301 = MISA
  | x == 0x302 = MEDeleg
  | x == 0x303 = MIDeleg
  | x == 0x304 = MIE
  | x == 0x305 = MTVec
  | x == 0x306 = MCounterEn
  | x == 0x340 = MScratch
  | x == 0x341 = MEPC
  | x == 0x342 = MCause
  | x == 0x343 = MTVal
  | x == 0x344 = MIP
  | x == 0xB00 = MCycle
  | x == 0xB02 = MInstRet
  | x == 0xB03 = MHPMCounter3
  | x == 0xB04 = MHPMCounter4
  | x == 0xB05 = MHPMCounter5
  | x == 0xB06 = MHPMCounter6
  | x == 0xB07 = MHPMCounter7
  | x == 0xB08 = MHPMCounter8
  | x == 0xB09 = MHPMCounter9
  | x == 0xB0A = MHPMCounter10
  | x == 0xB0B = MHPMCounter11
  | x == 0xB0C = MHPMCounter12
  | x == 0xB0D = MHPMCounter13
  | x == 0xB0E = MHPMCounter14
  | x == 0xB0F = MHPMCounter15
  | x == 0xB10 = MHPMCounter16
  | x == 0xB11 = MHPMCounter17
  | x == 0xB12 = MHPMCounter18
  | x == 0xB13 = MHPMCounter19
  | x == 0xB14 = MHPMCounter20
  | x == 0xB15 = MHPMCounter21
  | x == 0xB16 = MHPMCounter22
  | x == 0xB17 = MHPMCounter23
  | x == 0xB18 = MHPMCounter24
  | x == 0xB19 = MHPMCounter25
  | x == 0xB1A = MHPMCounter26
  | x == 0xB1B = MHPMCounter27
  | x == 0xB1C = MHPMCounter28
  | x == 0xB1D = MHPMCounter29
  | x == 0xB1E = MHPMCounter30
  | x == 0xB1F = MHPMCounter31
  | x == 0x100 = SStatus
  | x == 0x102 = SEDeleg
  | x == 0x103 = SIDeleg
  | x == 0x104 = SIE
  | x == 0x105 = STVec
  | x == 0x106 = SCounterEn
  | x == 0x140 = SScratch
  | x == 0x141 = SEPC
  | x == 0x142 = SCause
  | x == 0x143 = STVal
  | x == 0x144 = SIP
  | x == 0x180 = SATP
  | x == 0x000 = UStatus
  | x == 0x001 = FFlags
  | x == 0x002 = FRM
  | x == 0x003 = FCSR
  | x == 0x004 = UIE
  | x == 0x005 = UTVec
  | x == 0x040 = UScratch
  | x == 0x041 = UEPC
  | x == 0x042 = UCause
  | x == 0x043 = UTVal
  | x == 0x044 = UIP
  | x == 0xC00 = Cycle
  | x == 0xC01 = Time
  | x == 0xC02 = InstRet
  | x == 0xF14 = MHartID
  | otherwise = InvalidCSR
