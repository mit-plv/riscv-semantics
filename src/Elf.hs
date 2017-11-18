module Elf where
import qualified Data.ByteString as B
import Data.Elf
import Data.Word

readElf :: FilePath -> IO [(Int, Word8)]
readElf f = do
    bytes <- B.readFile f
    return $ translateElf $ parseElf bytes

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

