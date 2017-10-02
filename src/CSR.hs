module CSR where
import Utility
import Data.Int
import Data.Bits
import Data.Maybe

data CSR = MISA { base :: Int, extensions :: Int }
         | MStatus { sd :: Bool, vm :: Int, mxr :: Bool, pum :: Bool, mprv :: Bool, xs :: Int,
                     fs :: Int, mpp :: Int, hpp :: Int, spp :: Bool, mpie :: Bool, hpie :: Bool,
                     spie :: Bool, upie :: Bool, mie :: Bool, hie :: Bool, sie :: Bool, uie :: Bool }
         | MIE { meie :: Bool, seie :: Bool, ueie :: Bool, mtie :: Bool, stie :: Bool, utie :: Bool,
                 msie :: Bool, ssie :: Bool, usie :: Bool }
         | MTVec { base :: Int, mode :: Int }
         | MCause { interrupt :: Bool, exception :: Int }
         | MSimple { val :: Int }
         | MNotImplemented { val :: Int }
         deriving Show

csrMap :: [(Int, Int -> CSR)]
csrMap = [(0xF11, decodeMSimple), -- mvendorid
          (0xF12, decodeMSimple), -- marchid
          (0xF13, decodeMSimple), -- mimpid
          (0xF14, decodeMSimple), -- mhartid
          (0x300, decodeMStatus),
          (0x301, decodeMISA),
          (0x302, decodeMNotImplemented), -- medeleg
          (0x303, decodeMNotImplemented), -- mideleg
          (0x304, decodeMIE),
          (0x305, decodeMTVec),
          (0x306, decodeMNotImplemented), -- mcounteren
          (0x340, decodeMSimple), -- mscratch
          (0x341, decodeMSimple), -- mepc
          (0x342, decodeMCause),
          (0x343, decodeMSimple), -- mtval
          (0x344, decodeMNotImplemented)] -- mip

defaultCSRs :: [(Int, CSR)]
defaultCSRs = map (\(addr, f) -> (addr, f 0)) csrMap

decodeMISA v = MISA { base = bitSlice v 30 32, extensions = bitSlice v 0 26 }
decodeMIE v = MIE { meie = testBit v 11, seie = testBit v 9, ueie = testBit v 8, mtie = testBit v 7,
                      stie = testBit v 5, utie = testBit v 4, msie = testBit v 3, ssie = testBit v 1,
                      usie = testBit v 0 }
decodeMTVec v = MTVec { base = bitSlice v 2 32, mode = bitSlice v 0 2 }
decodeMStatus v = MStatus { sd = testBit v 31, vm = bitSlice v 24 29,
                              mxr = testBit v 19, pum = testBit v 18,
                              mprv = testBit v 17, xs = bitSlice v 15 17,
                              fs = bitSlice v 13 15, mpp = bitSlice v 11 13,
                              hpp = bitSlice v 9 11, spp = testBit v 8,
                              mpie = testBit v 7, hpie = testBit v 6,
                              spie = testBit v 5, upie = testBit v 4,
                              mie = testBit v 3, hie = testBit v 2,
                              sie = testBit v 1, uie = testBit v 0 }
decodeMCause v = MCause { interrupt = testBit v 31, exception = bitSlice v 0 31 }
decodeMSimple = MSimple
decodeMNotImplemented val = error "Not implemented yet."

decode :: Int -> Int32 -> CSR
decode idx = fromJust (lookup idx csrMap) . fromIntegral

boolBit :: Bool -> Int -> Int
boolBit b i = if b then bit i else zeroBits

encode :: CSR -> Int32
encode (MISA base extensions) = fromIntegral $ shift base 30 .|. extensions
encode (MStatus sd vm mxr pum mprv xs fs mpp hpp spp mpie hpie spie upie mie hie sie uie) = fromIntegral $
  boolBit sd 31 .|. shift vm 24 .|. boolBit mxr 19 .|. boolBit pum 18 .|. boolBit mprv 17 .|. shift xs 15 .|.
  shift fs 13 .|. shift mpp 11 .|. shift hpp 9 .|. boolBit spp 8 .|. boolBit mpie 7 .|. boolBit hpie 6 .|.
  boolBit spie 5 .|. boolBit upie 4 .|. boolBit mie 3 .|. boolBit hie 2 .|. boolBit sie 1 .|. boolBit uie 0
encode (MIE meie seie ueie mtie stie utie msie ssie usie) = fromIntegral $
  boolBit meie 11 .|. boolBit seie 9 .|. boolBit ueie 8 .|. boolBit mtie 7 .|. boolBit stie 5 .|.
  boolBit utie 4 .|. boolBit msie 3 .|. boolBit ssie 1 .|. boolBit usie 0
encode (MTVec base mode) = fromIntegral $ shift base 2 .|. mode
encode (MCause interrupt exception) = fromIntegral $ boolBit interrupt 31 .|. exception
encode (MSimple val) = fromIntegral val
encode (MNotImplemented val) = error "Not implemented yet."
