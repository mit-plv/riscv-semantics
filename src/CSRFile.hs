module CSRFile where
import CSRField
import Utility
import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe

type CSRFile = M.Map CSRField MachineInt

encodeExtensions :: String -> MachineInt
encodeExtensions extensions = foldr (.|.) 0 (map encode extensions)
  where encode c = shift 1 (fromJust (c `elemIndex` ['A'..'Z']))

emptyFile = M.empty

resetCSRFile :: Integer -> CSRFile
resetCSRFile 32 = M.fromList [(MXL, 1), (Extensions, encodeExtensions "IMSU")]
resetCSRFile 64 = M.fromList [(MXL, 2), (Extensions, encodeExtensions "IMSU")]

getField :: CSRField -> CSRFile -> MachineInt
getField field file = fromMaybe 0 (M.lookup field file)
setField :: CSRField -> MachineInt -> CSRFile -> CSRFile
setField = M.insert
