module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Tuple
import Prelude
import qualified Prelude as P

-- Machine-level CSRs.
data CSR = MISA | MStatus | MTVec | MEDeleg | MIDeleg | MIP | MIE | MCycle |
           MInstRet | MCounterEn | MScratch | MEPC | MCause | MTVal |
-- Supervisor-level CSRs
           SStatus | SEDeleg | SIDeleg | STVec | SIP | SIE | SScratch | SEPC |
           SCause | STVal | SATP |
-- User-mode CSRs
           UStatus | UIE | UTVec | UScratch | UEPC | UCause | UTVal | UIP |
           FFlags | FRM | FCSR | Time | Cycle | InstRet |
           InvalidCSR
  deriving Eq

csrTable = [(0x300, MStatus),
            (0x301, MISA),
            (0x302, MEDeleg),
            (0x303, MIDeleg),
            (0x304, MIE),
            (0x305, MTVec),
            (0x306, MCounterEn),
            (0x340, MScratch),
            (0x341, MEPC),
            (0x342, MCause),
            (0x343, MTVal),
            (0x344, MIP),
            (0xB00, MCycle),
            (0xB02, MInstRet),
            (0x100, SStatus),
            (0x102, SEDeleg),
            (0x103, SIDeleg),
            (0x104, SIE),
            (0x105, STVec),
            (0x140, SScratch),
            (0x141, SEPC),
            (0x142, SCause),
            (0x143, STVal),
            (0x144, SIP),
            (0x180, SATP),
            (0x000, UStatus),
            (0x001, FFlags),
            (0x002, FRM),
            (0x003, FCSR),
            (0x004, UIE),
            (0x005, UTVec),
            (0x040, UScratch),
            (0x041, UEPC),
            (0x042, UCause),
            (0x043, UTVal),
            (0x044, UIP),
            (0x001, FFlags),
            (0x002, FRM),
            (0x003, FCSR),
            (0xC00, Cycle),
            (0xC01, Time),
            (0xC02, InstRet)]

instance Enum CSR where
    fromEnum = fromMaybe 0x1000 . flip lookup (map swap csrTable)
    toEnum = fromMaybe InvalidCSR . flip lookup csrTable

-- For Clash's sake.
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
lookupCSR 0x100 = SStatus
lookupCSR 0x102 = SEDeleg
lookupCSR 0x103 = SIDeleg
lookupCSR 0x104 = SIE
lookupCSR 0x105 = STVec
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
lookupCSR 0x001 = FFlags
lookupCSR 0x002 = FRM
lookupCSR 0x003 = FCSR
lookupCSR 0xC00 = Cycle
lookupCSR 0xC01 = Time
lookupCSR 0xC02 = InstRet
lookupCSR _ = InvalidCSR
