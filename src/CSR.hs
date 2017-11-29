module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Tuple
import Prelude

data CSR = MISA | MStatus | MTVec | MEDeleg | MIDeleg | MIP | MIE | MCycle | MInstRet |
           MCounterEn | MScratch | MEPC | MCause | MTVal | InvalidCSR
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
            (0xB02, MInstRet)]

instance Enum CSR where
    fromEnum = fromMaybe 0x1000 . flip lookup (map swap csrTable)
    toEnum = fromMaybe InvalidCSR . flip lookup csrTable

lookupCSR :: (Integral x) => x -> CSR
lookupCSR = toEnum . fromIntegral
