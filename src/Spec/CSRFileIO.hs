module Spec.CSRFileIO where
import Spec.CSRField
import Data.Array.IO
import Utility.Utility
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

type CSRFile =  IOUArray CSRField MachineInt

encodeExtensions :: String -> MachineInt
encodeExtensions extensions = foldr (.|.) 0 (map encode extensions)
  where encode c = shift 1 (fromJust (c `elemIndex` ['A'..'Z']))
