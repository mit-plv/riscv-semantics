module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Tuple
import Prelude
import qualified Prelude as P

-- Machine-mode CSRs.
data CSR = MISA | MStatus | MTVec | MEDeleg | MIDeleg | MIP | MIE | MCycle |
           MInstRet | MCounterEn | MScratch | MEPC | MCause | MTVal |
-- Supervisor-mode CSRs
           SATP |
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
            (0x180, SATP)]

instance Enum CSR where
    fromEnum = fromMaybe 0x1000 . flip lookup (map swap csrTable)
    toEnum = fromMaybe InvalidCSR . flip lookup csrTable

lookupCSR :: (Integral x) => x -> CSR
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
      | x == 0x180 = SATP
      | otherwise = InvalidCSR
