module CSRFile where
import CSRField
import Utility
import qualified Data.Map as M
import Data.Maybe

type CSRFile = M.Map CSRField MachineInt

emptyFile = M.empty

resetCSRFile :: Integer -> CSRFile
resetCSRFile 32 = M.fromList [(MXL, 1)]
resetCSRFile 64 = M.fromList [(MXL, 2)]

getField :: CSRField -> CSRFile -> MachineInt
getField field file = fromMaybe 0 (M.lookup field file)
setField :: CSRField -> MachineInt -> CSRFile -> CSRFile
setField = M.insert
