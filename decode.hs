import Data.Int
import Data.Bits
import Data.Maybe
import Data.List

data Instruction =
  Lui { rd :: Int32, imm20 :: Int32 } |
  Auipc { rd :: Int32, oimm20 :: Int32 } |
  Jal { rd :: Int32, jimm20 :: Int32 } |
  Jalr { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Beq { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Bne { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Blt { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Bge { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Bltu { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Bgeu { rs1 :: Int32, rs2 :: Int32, sbimm12 :: Int32 } |
  Lb { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Lh { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Lw { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Lbu { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Lhu { rd :: Int32, rs1 :: Int32, oimm12 :: Int32 } |
  Sb { rs1 :: Int32, rs2 :: Int32, simm12 :: Int32 } |
  Sh { rs1 :: Int32, rs2 :: Int32, simm12 :: Int32 } |
  Sw { rs1 :: Int32, rs2 :: Int32, simm12 :: Int32 } |
  Addi { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Slti { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Sltiu { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Xori { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Ori { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Andi { rd :: Int32, rs1 :: Int32, imm12 :: Int32 } |
  Slli { rd :: Int32, rs1 :: Int32, shamt5 :: Int32 } |
  Srli { rd :: Int32, rs1 :: Int32, shamt5 :: Int32 } |
  Srai { rd :: Int32, rs1 :: Int32, shamt5 :: Int32 } |
  Add { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Sub { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Sll { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Slt { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Sltu { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Xor { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Srl { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Sra { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Or { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  And { rd :: Int32, rs1 :: Int32, rs2 :: Int32 } |
  Fence { pred :: Int32, succ :: Int32 } |
  Fence_i
  deriving (Eq, Read, Show)

bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (.&.) (shiftR x start) (complement $ shiftL (-1) (end - start))

getRd inst = bitSlice inst 7 12
getRs1 inst = bitSlice inst 15 20
getRs2 inst = bitSlice inst 20 25
getPred inst = bitSlice inst 24 28
getSucc inst = bitSlice inst 20 24
getImm20 inst = shift (bitSlice inst 12 32) 12
getOimm20 inst = shift (bitSlice inst 12 32) 12
getJimm20 inst = (.|.) (shift (bitSlice inst 31 32) 20) $ (.|.) (shift (bitSlice inst 21 31) 1) $
                 (.|.) (shift (bitSlice inst 20 21) 11) (shift (bitSlice inst 12 20) 12)
getImm12 inst = bitSlice inst 20 32
getOimm12 inst = bitSlice inst 20 32
getCsr12 inst = bitSlice inst 20 32
getSimm12 inst = (.|.) (shift (bitSlice inst 25 32) 5) (bitSlice inst 7 12)
getSbimm12 inst = (.|.) (shift (bitSlice inst 31 32) 12) $ (.|.) (shift (bitSlice inst 25 31) 5) $
                  (.|.) (shift (bitSlice inst 8 12) 1) (shift (bitSlice inst 7 8) 11)
getShamt5 inst = bitSlice inst 20 25

decodeLui inst = Lui (getRd inst) (getImm20 inst)
decodeAuipc inst = Auipc (getRd inst) (getOimm20 inst)
decodeJal inst = Jal (getRd inst) (getJimm20 inst)
decodeJalr inst = Jalr (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeBeq inst = Beq (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeBne inst = Bne (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeBlt inst = Blt (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeBge inst = Bge (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeBltu inst = Bltu (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeBgeu inst = Bgeu (getRs1 inst) (getRs2 inst) (getSbimm12 inst)
decodeLb inst = Lb (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeLh inst = Lh (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeLw inst = Lw (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeLbu inst = Lbu (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeLhu inst = Lhu (getRd inst) (getRs1 inst) (getOimm12 inst)
decodeSb inst = Sb (getRs1 inst) (getRs2 inst) (getSimm12 inst)
decodeSh inst = Sh (getRs1 inst) (getRs2 inst) (getSimm12 inst)
decodeSw inst = Sw (getRs1 inst) (getRs2 inst) (getSimm12 inst)
decodeAddi inst = Addi (getRd inst) (getRs1 inst) (getImm12 inst)
decodeSlti inst = Slti (getRd inst) (getRs1 inst) (getImm12 inst)
decodeSltiu inst = Sltiu (getRd inst) (getRs1 inst) (getImm12 inst)
decodeXori inst = Xori (getRd inst) (getRs1 inst) (getImm12 inst)
decodeOri inst = Ori (getRd inst) (getRs1 inst) (getImm12 inst)
decodeAndi inst = Andi (getRd inst) (getRs1 inst) (getImm12 inst)
decodeSlli inst = Slli (getRd inst) (getRs1 inst) (getShamt5 inst)
decodeSrli inst = Srli (getRd inst) (getRs1 inst) (getShamt5 inst)
decodeSrai inst = Srai (getRd inst) (getRs1 inst) (getShamt5 inst)
decodeAdd inst = Add (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSub inst = Sub (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSll inst = Sll (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSlt inst = Slt (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSltu inst = Sltu (getRd inst) (getRs1 inst) (getRs2 inst)
decodeXor inst = Xor (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSrl inst = Srl (getRd inst) (getRs1 inst) (getRs2 inst)
decodeSra inst = Sra (getRd inst) (getRs1 inst) (getRs2 inst)
decodeOr inst = Or (getRd inst) (getRs1 inst) (getRs2 inst)
decodeAnd inst = And (getRd inst) (getRs1 inst) (getRs2 inst)
decodeFence inst = Fence (getPred inst) (getSucc inst)
decodeFence_i inst = Fence_i

-- (decodeLui, [(2, 7, 0x0D), (0, 2, 3)]) corresponds to (lui 6..2=0x0D 1..0=3) in opcodes file.
opcodeTable :: [(Int32 -> Instruction, [(Int, Int, Int32)])]
opcodeTable = [(decodeLui, [(2,7,0x0D),(0,2,3)]),
               (decodeAuipc, [(2,7,0x05),(0,2,3)]),
               (decodeJal, [(2,7,0x1b),(0,2,3)]),
               (decodeJalr, [(12,15,0),(2,7,0x19),(0,2,3)]),
               (decodeBeq, [(12,15,0),(2,7,0x18),(0,2,3)]),
               (decodeBne, [(12,15,1),(2,7,0x18),(0,2,3)]),
               (decodeBlt, [(12,15,4),(2,7,0x18),(0,2,3)]),
               (decodeBge, [(12,15,5),(2,7,0x18),(0,2,3)]),
               (decodeBltu, [(12,15,6),(2,7,0x18),(0,2,3)]),
               (decodeBgeu, [(12,15,7),(2,7,0x18),(0,2,3)]),
               (decodeLb, [(12,15,0),(2,7,0x00),(0,2,3)]),
               (decodeLh, [(12,15,1),(2,7,0x00),(0,2,3)]),
               (decodeLw, [(12,15,2),(2,7,0x00),(0,2,3)]),
               (decodeLbu, [(12,15,4),(2,7,0x00),(0,2,3)]),
               (decodeLhu, [(12,15,5),(2,7,0x00),(0,2,3)]),
               (decodeSb, [(12,15,0),(2,7,0x08),(0,2,3)]),
               (decodeSh, [(12,15,1),(2,7,0x08),(0,2,3)]),
               (decodeSw, [(12,15,2),(2,7,0x08),(0,2,3)]),
               (decodeAddi, [(12,15,0),(2,7,0x04),(0,2,3)]),
               (decodeSlti, [(12,15,2),(2,7,0x04),(0,2,3)]),
               (decodeSltiu, [(12,15,3),(2,7,0x04),(0,2,3)]),
               (decodeXori, [(12,15,4),(2,7,0x04),(0,2,3)]),
               (decodeOri, [(12,15,6),(2,7,0x04),(0,2,3)]),
               (decodeAndi, [(12,15,7),(2,7,0x04),(0,2,3)]),
               (decodeSlli, [(27,32,0),(12,15,1),(2,7,0x04),(0,2,3)]),
               (decodeSrli, [(27,32,0),(12,15,5),(2,7,0x04),(0,2,3)]),
               (decodeSrai, [(27,32,8),(12,15,5),(2,7,0x04),(0,2,3)]),
               (decodeAdd, [(25,32,0),(12,15,0),(2,7,0x0C),(0,2,3)]),
               (decodeSub, [(25,32,32),(12,15,0),(2,7,0x0C),(0,2,3)]),
               (decodeSll, [(25,32,0),(12,15,1),(2,7,0x0C),(0,2,3)]),
               (decodeSlt, [(25,32,0),(12,15,2),(2,7,0x0C),(0,2,3)]),
               (decodeSltu, [(25,32,0),(12,15,3),(2,7,0x0C),(0,2,3)]),
               (decodeXor, [(25,32,0),(12,15,4),(2,7,0x0C),(0,2,3)]),
               (decodeSrl, [(25,32,0),(12,15,5),(2,7,0x0C),(0,2,3)]),
               (decodeSra, [(25,32,32),(12,15,5),(2,7,0x0C),(0,2,3)]),
               (decodeOr, [(25,32,0),(12,15,6),(2,7,0x0C),(0,2,3)]),
               (decodeAnd, [(25,32,0),(12,15,7),(2,7,0x0C),(0,2,3)]),
               (decodeFence, [(12,15,0),(2,7,0x03),(0,2,3)]),
               (decodeFence_i, [(12,15,1),(2,7,0x03),(0,2,3)])]

decode :: Int32 -> Instruction
decode inst = (fst $ fromJust $ find (\e -> all match (snd e)) opcodeTable) inst
              where match (start, end, val) = bitSlice inst start end == val
