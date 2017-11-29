module Elf where
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Elf
import Data.Word

-- External functions
-- readElf returns the data loaded by an elf file as a list of tuples of byte
-- addresses and bytes of data.
readElf :: FilePath -> IO [(Int, Word8)]
readElf f = do
    bytes <- B.readFile f
    return $ translateElf $ parseElf bytes

-- readElfSymbol takes a symbol name and returns its address if that symbol
-- exists in the elf file.
readElfSymbol :: String -> FilePath -> IO (Maybe Word64)
readElfSymbol symbolName f = do
    bytes <- B.readFile f
    return $ findSymbolAddress symbolName $ parseElf bytes

-- Internal functions
translateElf :: Elf -> [(Int, Word8)]
translateElf e = concat $ map translateElfSegment $ elfSegments e

translateElfSegment :: ElfSegment -> [(Int, Word8)]
translateElfSegment s =
    if elfSegmentType s == PT_LOAD
        then addressEachByte (fromIntegral $ elfSegmentPhysAddr s) (B.unpack $ elfSegmentData s)
        else []

addressEachByte :: Int -> [Word8] -> [(Int, Word8)]
addressEachByte addr (b:bs) = (addr, b) : (addressEachByte (addr+1) bs)
addressEachByte addr [] = []

findSymbolAddress :: String -> Elf -> Maybe Word64
findSymbolAddress symbolName elf =
    foldl mplus Nothing $ map (symbolTableFilter symbolName) $ concat $ parseSymbolTables elf

symbolTableFilter :: String -> ElfSymbolTableEntry -> Maybe Word64
symbolTableFilter symbolName ste =
    if (snd (steName ste)) == (Just $ BC8.pack symbolName)
        then Just $ steValue ste
        else Nothing
