module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe

data CSR = MISA { base :: Int, extensions :: Int }
         | MStatus { sd :: Bool, vm :: Int, mxr :: Bool, pum :: Bool, mprv :: Bool, xs :: Int,
                     fs :: Int, mpp :: Int, hpp :: Int, spp :: Bool, mpie :: Bool, hpie :: Bool,
                     spie :: Bool, upie :: Bool, mie :: Bool, hie :: Bool, sie :: Bool, uie :: Bool }
         | MOther { val :: Int }
         deriving Show

csrMap :: [(Int, Int32 -> CSR)]
csrMap = [(0xF11, decodeOther),
          (0x300, decodeMStatus),
          (0x301, decodeMISA)]

defaultCSRs :: [(Int, CSR)]
defaultCSRs = map (\(addr, f) -> (addr, f 0)) csrMap

decodeOther = MOther . fromIntegral
decodeMISA val = MISA { base = bitSlice v 30 32, extensions = bitSlice v 0 26 }
  where v = fromIntegral val
decodeMStatus val = MStatus { sd = testBit v 31, vm = bitSlice v 24 29,
                              mxr = testBit v 19, pum = testBit v 18,
                              mprv = testBit v 17, xs = bitSlice v 15 17,
                              fs = bitSlice v 13 15, mpp = bitSlice v 11 13,
                              hpp = bitSlice v 9 11, spp = testBit v 8,
                              mpie = testBit v 7, hpie = testBit v 6,
                              spie = testBit v 5, upie = testBit v 4,
                              mie = testBit v 3, hie = testBit v 2,
                              sie = testBit v 1, uie = testBit v 0 }
  where v = fromIntegral val

decode :: Int -> Int32 -> CSR
decode idx = fromJust (lookup idx csrMap)

boolBit :: (Bits a) => Bool -> Int -> a
boolBit b i = if b then bit i else zeroBits

encode :: CSR -> Int32
encode (MISA base extensions) = fromIntegral $ shift base 30 .|. extensions
encode (MStatus sd vm mxr pum mprv xs fs mpp hpp spp mpie hpie spie upie mie hie sie uie) = fromIntegral $
  boolBit sd 31 .|. shift vm 24 .|. boolBit mxr 19 .|. boolBit pum 18 .|. boolBit mprv 17 .|. shift xs 15 .|.
  shift fs 13 .|. shift mpp 11 .|. shift hpp 9 .|. boolBit spp 8 .|. boolBit mpie 7 .|. boolBit hpie 6 .|.
  boolBit spie 5 .|. boolBit upie 4 .|. boolBit mie 3 .|. boolBit hie 2 .|. boolBit sie 1 .|. boolBit uie 0
encode (MOther val) = fromIntegral val
