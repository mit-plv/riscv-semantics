module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Tuple

data CSR = MISA | MStatus | MTVec | MEDeleg | MIDeleg | MIP | MIE | MCycle | MInstRet |
           MCounterEn | MScratch | MEPC | MCause | MTVal
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
            (0x344, MIP)]

instance Enum CSR where
    fromEnum = fromJust . flip lookup (map swap csrTable)
    toEnum = fromJust . flip lookup csrTable

lookupCSR :: (Integral x) => x -> CSR
lookupCSR = toEnum . fromIntegral
