{-# LANGUAGE DuplicateRecordFields, BinaryLiterals #-}
module Spec.Decode where

-- Haskell lib imports

import Data.Bits hiding (Xor, And, Or)   -- For bit-wise 'and' (.&.) etc.
import Data.Word    -- For Word32 type (unsigned 32-bit ints)
import Prelude
-- Local imports

import Utility.Utility

-- ================================================================
-- Decoded instructions

data InstructionI =
  Lb { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Lh { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Lw { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Lbu { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Lhu { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |

  Fence { pred :: MachineInt, succ :: MachineInt } |
  Fence_i |

  Addi { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Slli { rd :: Register, rs1 :: Register, shamt6 :: Int } |
  Slti { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Sltiu { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Xori { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Ori { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Andi { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Srli { rd :: Register, rs1 :: Register, shamt6 :: Int } |
  Srai { rd :: Register, rs1 :: Register, shamt6 :: Int } |

  Auipc { rd :: Register, oimm20 :: MachineInt } |

  Sb { rs1 :: Register, rs2 :: Register, simm12 :: MachineInt } |
  Sh { rs1 :: Register, rs2 :: Register, simm12 :: MachineInt } |
  Sw { rs1 :: Register, rs2 :: Register, simm12 :: MachineInt } |

  Add { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sub { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sll { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Slt { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sltu { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Xor { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Srl { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sra { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Or { rd :: Register, rs1 :: Register, rs2 :: Register } |
  And { rd :: Register, rs1 :: Register, rs2 :: Register } |

  Lui { rd :: Register, imm20 :: MachineInt } |

  Beq { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |
  Bne { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |
  Blt { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |
  Bge { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |
  Bltu { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |
  Bgeu { rs1 :: Register, rs2 :: Register, sbimm12 :: MachineInt } |

  Jalr { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Jal { rd :: Register, jimm20 :: MachineInt } |

  InvalidI
  deriving (Eq, Read, Show)


data InstructionM =
  Mul { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Mulh { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Mulhsu { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Mulhu { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Div { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Divu { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Rem { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Remu { rd :: Register, rs1 :: Register, rs2 :: Register } |
  InvalidM
  deriving (Eq, Read, Show)


data InstructionA =
  Lr_w { rd :: Register, rs1 :: Register, aqrl :: MachineInt } |
  Sc_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |

  Amoswap_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoadd_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoand_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoor_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoxor_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomax_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomaxu_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomin_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amominu_w { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |

  InvalidA
  deriving (Eq, Read, Show)


data InstructionF =
  Flw { rd :: FPRegister, rs1 :: Register, oimm12 :: MachineInt } |
  Fsw { rs1 :: Register, rs2 :: FPRegister, simm12 :: MachineInt } |
  Fmadd_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rs3 :: FPRegister, rm :: RoundMode } |
  Fmsub_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rs3 :: FPRegister, rm :: RoundMode } |
  Fnmsub_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rs3 :: FPRegister, rm :: RoundMode } |
  Fnmadd_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rs3 :: FPRegister, rm :: RoundMode } |
  Fadd_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rm :: RoundMode } |
  Fsub_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rm :: RoundMode } |
  Fmul_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rm :: RoundMode } |
  Fdiv_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister, rm :: RoundMode } |
  Fsqrt_s { rd :: FPRegister, rs1 :: FPRegister, rm :: RoundMode } |
  Fsgnj_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fsgnjn_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fsgnjx_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fmin_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fmax_s { rd :: FPRegister, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fcvt_w_s { rd :: Register, rs1 :: FPRegister, rm :: RoundMode } |
  Fcvt_wu_s { rd :: Register, rs1 :: FPRegister, rm :: RoundMode } |
  Fmv_x_w { rd :: Register, rs1 :: FPRegister } |
  Feq_s { rd :: Register, rs1 :: FPRegister, rs2 :: FPRegister } |
  Flt_s { rd :: Register, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fle_s { rd :: Register, rs1 :: FPRegister, rs2 :: FPRegister } |
  Fclass_s { rd :: Register, rs1 :: FPRegister } |
  Fcvt_s_w { rd :: FPRegister, rs1 :: Register, rm :: RoundMode } |
  Fcvt_s_wu { rd :: FPRegister, rs1 :: Register, rm :: RoundMode } |
  Fmv_w_x { rd :: FPRegister, rs1 :: Register } |
  InvalidF
  deriving (Eq, Read, Show)


data InstructionI64 =
  Ld { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |
  Lwu { rd :: Register, rs1 :: Register, oimm12 :: MachineInt } |

  Addiw { rd :: Register, rs1 :: Register, imm12 :: MachineInt } |
  Slliw { rd :: Register, rs1 :: Register, shamt5 :: Int } |
  Srliw { rd :: Register, rs1 :: Register, shamt5 :: Int } |
  Sraiw { rd :: Register, rs1 :: Register, shamt5 :: Int } |

  Sd { rs1 :: Register, rs2 :: Register, simm12 :: MachineInt } |

  Addw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Subw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sllw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Srlw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Sraw { rd :: Register, rs1 :: Register, rs2 :: Register } |

  InvalidI64
  deriving (Eq, Read, Show)


data InstructionM64 =
  Mulw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Divw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Divuw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Remw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  Remuw { rd :: Register, rs1 :: Register, rs2 :: Register } |
  InvalidM64
  deriving (Eq, Read, Show)


data InstructionA64 =
  Lr_d { rd :: Register, rs1 :: Register, aqrl :: MachineInt } |
  Sc_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |

  Amoswap_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoadd_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoand_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoor_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amoxor_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomax_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomaxu_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amomin_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |
  Amominu_d { rd :: Register, rs1 :: Register, rs2 :: Register, aqrl :: MachineInt } |

  InvalidA64
  deriving (Eq, Read, Show)


data InstructionF64 =
  Fcvt_l_s { rd :: Register, rs1 :: FPRegister, rm :: RoundMode } |
  Fcvt_lu_s { rd :: Register, rs1 :: FPRegister, rm :: RoundMode } |
  Fcvt_s_l { rd :: FPRegister, rs1 :: Register, rm :: RoundMode } |
  Fcvt_s_lu { rd :: FPRegister, rs1 :: Register, rm :: RoundMode } |
  InvalidF64
  deriving (Eq, Read, Show)


data InstructionCSR =
  Ecall |
  Ebreak |
  Uret |
  Sret |
  Mret |
  Wfi |
  Sfence_vma { rs1 :: Register, rs2 :: Register } |
  Csrrw { rd :: Register, rs1 :: Register, csr12 :: MachineInt } |
  Csrrs { rd :: Register, rs1 :: Register, csr12 :: MachineInt } |
  Csrrc { rd :: Register, rs1 :: Register, csr12 :: MachineInt } |
  Csrrwi { rd :: Register, zimm :: MachineInt, csr12 :: MachineInt } |
  Csrrsi { rd :: Register, zimm :: MachineInt, csr12 :: MachineInt } |
  Csrrci { rd :: Register, zimm :: MachineInt, csr12 :: MachineInt } |
  InvalidCSR
  deriving (Eq, Read, Show)


data Instruction =
  IInstruction   { iInstruction   :: InstructionI   } |
  MInstruction   { mInstruction   :: InstructionM   } |
  AInstruction   { aInstruction   :: InstructionA   } |
  FInstruction   { fInstruction   :: InstructionF   } |
  I64Instruction { i64Instruction :: InstructionI64 } |
  M64Instruction { m64Instruction :: InstructionM64 } |
  A64Instruction { a64Instruction :: InstructionA64 } |
  F64Instruction { f64Instruction :: InstructionF64 } |
  CSRInstruction { csrInstruction :: InstructionCSR } |
  InvalidInstruction { inst :: MachineInt }
  deriving (Eq, Read, Show)

-- ================================================================

-- TODO: Switch to a representation that doesn't involve enumerating all the
-- combinatoric possibilities; possibily the one used by the MISA CSR.
data InstructionSet = RV32I | RV32IM | RV32IA | RV32IMA | RV32IF | RV32IMF | RV32IAF | RV32IMAF |
                      RV64I | RV64IM | RV64IA | RV64IMA | RV64IF | RV64IMF | RV64IAF | RV64IMAF
  deriving (Eq, Show)

bitwidth :: InstructionSet -> Int
bitwidth RV32I = 32
bitwidth RV32IM = 32
bitwidth RV32IA = 32
bitwidth RV32IMA = 32
bitwidth RV32IF = 32
bitwidth RV32IMF = 32
bitwidth RV32IAF = 32
bitwidth RV32IMAF = 32
bitwidth RV64I = 64
bitwidth RV64IM = 64
bitwidth RV64IA = 64
bitwidth RV64IMA = 64
bitwidth RV64IF = 64
bitwidth RV64IMF = 64
bitwidth RV64IAF = 64
bitwidth RV64IMAF = 64

supportsM :: InstructionSet -> Bool
supportsM RV32IM = True
supportsM RV32IMA = True
supportsM RV32IMF = True
supportsM RV32IMAF = True
supportsM RV64IM = True
supportsM RV64IMA = True
supportsM RV64IMF = True
supportsM RV64IMAF = True
supportsM _ = False

supportsA :: InstructionSet -> Bool
supportsA RV32IA = True
supportsA RV32IMA = True
supportsA RV32IAF = True
supportsA RV32IMAF = True
supportsA RV64IA = True
supportsA RV64IMA = True
supportsA RV64IAF = True
supportsA RV64IMAF = True
supportsA _ = False

supportsF :: InstructionSet -> Bool
supportsF RV32IF = True
supportsF RV32IMF = True
supportsF RV32IAF = True
supportsF RV32IMAF = True
supportsF RV64IF = True
supportsF RV64IMF = True
supportsF RV64IAF = True
supportsF RV64IMAF = True
supportsF _ = False

-- ================================================================

type Register = MachineInt
type FPRegister = MachineInt
type RoundMode = MachineInt

-- ================================================================
-- Instruction bit fields

-- Major opcode (instr [6:0]), see Table 19.1 in spec
type Opcode = MachineInt

opcode_LOAD      :: Opcode;    opcode_LOAD      = 0b0000011
opcode_LOAD_FP   :: Opcode;    opcode_LOAD_FP   = 0b0000111
opcode_MISC_MEM  :: Opcode;    opcode_MISC_MEM  = 0b0001111
opcode_OP_IMM    :: Opcode;    opcode_OP_IMM    = 0b0010011
opcode_AUIPC     :: Opcode;    opcode_AUIPC     = 0b0010111
opcode_OP_IMM_32 :: Opcode;    opcode_OP_IMM_32 = 0b0011011

opcode_STORE     :: Opcode;    opcode_STORE     = 0b0100011
opcode_STORE_FP  :: Opcode;    opcode_STORE_FP  = 0b0100111
opcode_AMO       :: Opcode;    opcode_AMO       = 0b0101111
opcode_OP        :: Opcode;    opcode_OP        = 0b0110011
opcode_LUI       :: Opcode;    opcode_LUI       = 0b0110111
opcode_OP_32     :: Opcode;    opcode_OP_32     = 0b0111011

opcode_MADD      :: Opcode;    opcode_MADD      = 0b1000011
opcode_MSUB      :: Opcode;    opcode_MSUB      = 0b1000111
opcode_NMSUB     :: Opcode;    opcode_NMSUB     = 0b1001011
opcode_NMADD     :: Opcode;    opcode_NMADD     = 0b1001111
opcode_OP_FP     :: Opcode;    opcode_OP_FP     = 0b1010011

opcode_BRANCH    :: Opcode;    opcode_BRANCH    = 0b1100011
opcode_JALR      :: Opcode;    opcode_JALR      = 0b1100111
opcode_JAL       :: Opcode;    opcode_JAL       = 0b1101111
opcode_SYSTEM    :: Opcode;    opcode_SYSTEM    = 0b1110011

-- LOAD sub-opcodes
funct3_LB  :: MachineInt;    funct3_LB  = 0b000
funct3_LH  :: MachineInt;    funct3_LH  = 0b001
funct3_LW  :: MachineInt;    funct3_LW  = 0b010
funct3_LD  :: MachineInt;    funct3_LD  = 0b011
funct3_LBU :: MachineInt;    funct3_LBU = 0b100
funct3_LHU :: MachineInt;    funct3_LHU = 0b101
funct3_LWU :: MachineInt;    funct3_LWU = 0b110

-- MISC_MEM sub-opcodes
funct3_FENCE   :: MachineInt;    funct3_FENCE   = 0b000
funct3_FENCE_I :: MachineInt;    funct3_FENCE_I = 0b001

-- OP_IMM sub-opcodes
funct3_ADDI  :: MachineInt;    funct3_ADDI  = 0b000
funct3_SLLI  :: MachineInt;    funct3_SLLI  = 0b001
funct3_SLTI  :: MachineInt;    funct3_SLTI  = 0b010
funct3_SLTIU :: MachineInt;    funct3_SLTIU = 0b011
funct3_XORI  :: MachineInt;    funct3_XORI  = 0b100
funct3_SRLI  :: MachineInt;    funct3_SRLI  = 0b101
funct3_SRAI  :: MachineInt;    funct3_SRAI  = 0b101
funct3_ORI   :: MachineInt;    funct3_ORI   = 0b110
funct3_ANDI  :: MachineInt;    funct3_ANDI  = 0b111

-- OP_IMM.SLLI/SRLI/SRAI
funct6_SLLI  :: MachineInt;    funct6_SLLI  = 0b000000
funct6_SRLI  :: MachineInt;    funct6_SRLI  = 0b000000
funct6_SRAI  :: MachineInt;    funct6_SRAI  = 0b010000

-- OP_IMM_32 sub-opcodes (RV64 only)
funct3_ADDIW :: MachineInt;    funct3_ADDIW = 0b000

funct3_SLLIW :: MachineInt;    funct3_SLLIW = 0b001
funct7_SLLIW :: MachineInt;    funct7_SLLIW = 0b0000000

funct3_SRLIW :: MachineInt;    funct3_SRLIW = 0b101
funct7_SRLIW :: MachineInt;    funct7_SRLIW = 0b0000000

funct3_SRAIW :: MachineInt;    funct3_SRAIW = 0b101
funct7_SRAIW :: MachineInt;    funct7_SRAIW = 0b0100000

-- STORE sub-opcodes
funct3_SB :: MachineInt;    funct3_SB = 0b000
funct3_SH :: MachineInt;    funct3_SH = 0b001
funct3_SW :: MachineInt;    funct3_SW = 0b010
funct3_SD :: MachineInt;    funct3_SD = 0b011

-- OP sub-opcodes
funct3_ADD  :: MachineInt;    funct3_ADD  = 0b000
funct7_ADD  :: MachineInt;    funct7_ADD  = 0b0000000

funct3_SUB  :: MachineInt;    funct3_SUB  = 0b000
funct7_SUB  :: MachineInt;    funct7_SUB  = 0b0100000

funct3_SLL  :: MachineInt;    funct3_SLL  = 0b001
funct7_SLL  :: MachineInt;    funct7_SLL  = 0b0000000

funct3_SLT  :: MachineInt;    funct3_SLT  = 0b010
funct7_SLT  :: MachineInt;    funct7_SLT  = 0b0000000

funct3_SLTU :: MachineInt;    funct3_SLTU = 0b011
funct7_SLTU :: MachineInt;    funct7_SLTU = 0b0000000

funct3_XOR  :: MachineInt;    funct3_XOR  = 0b100
funct7_XOR  :: MachineInt;    funct7_XOR  = 0b0000000

funct3_SRL  :: MachineInt;    funct3_SRL  = 0b101
funct7_SRL  :: MachineInt;    funct7_SRL  = 0b0000000

funct3_SRA  :: MachineInt;    funct3_SRA  = 0b101
funct7_SRA  :: MachineInt;    funct7_SRA  = 0b0100000

funct3_OR   :: MachineInt;    funct3_OR   = 0b110
funct7_OR   :: MachineInt;    funct7_OR   = 0b0000000

funct3_AND  :: MachineInt;    funct3_AND  = 0b111
funct7_AND  :: MachineInt;    funct7_AND  = 0b0000000

-- OP sub-opcodes, M standard extension

funct3_MUL    :: MachineInt;    funct3_MUL    = 0b000
funct7_MUL    :: MachineInt;    funct7_MUL    = 0b0000001

funct3_MULH   :: MachineInt;    funct3_MULH   = 0b001
funct7_MULH   :: MachineInt;    funct7_MULH   = 0b0000001

funct3_MULHSU :: MachineInt;    funct3_MULHSU = 0b010
funct7_MULHSU :: MachineInt;    funct7_MULHSU = 0b0000001

funct3_MULHU  :: MachineInt;    funct3_MULHU  = 0b011
funct7_MULHU  :: MachineInt;    funct7_MULHU  = 0b0000001

funct3_DIV    :: MachineInt;    funct3_DIV    = 0b100
funct7_DIV    :: MachineInt;    funct7_DIV    = 0b0000001

funct3_DIVU   :: MachineInt;    funct3_DIVU   = 0b101
funct7_DIVU   :: MachineInt;    funct7_DIVU   = 0b0000001

funct3_REM    :: MachineInt;    funct3_REM    = 0b110
funct7_REM    :: MachineInt;    funct7_REM    = 0b0000001

funct3_REMU   :: MachineInt;    funct3_REMU   = 0b111
funct7_REMU   :: MachineInt;    funct7_REMU   = 0b0000001

-- OP_32 sub-opcodes (RV64 only)
funct3_ADDW  :: MachineInt;    funct3_ADDW  = 0b000
funct7_ADDW  :: MachineInt;    funct7_ADDW  = 0b0000000

funct3_SUBW  :: MachineInt;    funct3_SUBW  = 0b000
funct7_SUBW  :: MachineInt;    funct7_SUBW  = 0b0100000

funct3_SLLW  :: MachineInt;    funct3_SLLW  = 0b001
funct7_SLLW  :: MachineInt;    funct7_SLLW  = 0b0000000

funct3_SRLW  :: MachineInt;    funct3_SRLW  = 0b101
funct7_SRLW  :: MachineInt;    funct7_SRLW  = 0b0000000

funct3_SRAW  :: MachineInt;    funct3_SRAW  = 0b101
funct7_SRAW  :: MachineInt;    funct7_SRAW  = 0b0100000

-- OP_32 sub-opcodes, M standard extension (RV64 only)
funct3_MULW  :: MachineInt;    funct3_MULW  = 0b000
funct7_MULW  :: MachineInt;    funct7_MULW  = 0b0000001

funct3_DIVW  :: MachineInt;    funct3_DIVW  = 0b100
funct7_DIVW  :: MachineInt;    funct7_DIVW  = 0b0000001

funct3_DIVUW :: MachineInt;    funct3_DIVUW = 0b101
funct7_DIVUW :: MachineInt;    funct7_DIVUW = 0b0000001

funct3_REMW  :: MachineInt;    funct3_REMW  = 0b110
funct7_REMW  :: MachineInt;    funct7_REMW  = 0b0000001

funct3_REMUW :: MachineInt;    funct3_REMUW = 0b111
funct7_REMUW :: MachineInt;    funct7_REMUW = 0b0000001

-- BRANCH sub-opcodes
funct3_BEQ  :: MachineInt;    funct3_BEQ  = 0b000
funct3_BNE  :: MachineInt;    funct3_BNE  = 0b001
funct3_BLT  :: MachineInt;    funct3_BLT  = 0b100
funct3_BGE  :: MachineInt;    funct3_BGE  = 0b101
funct3_BLTU :: MachineInt;    funct3_BLTU = 0b110
funct3_BGEU :: MachineInt;    funct3_BGEU = 0b111
funct3_JALR :: MachineInt;    funct3_JALR = 0b000

-- SYSTEM sub-opcodes
funct3_PRIV   :: MachineInt;    funct3_PRIV   = 0b000
--- SYSTEM.PRIV sub-sub-opcodes
funct12_ECALL    :: MachineInt;    funct12_ECALL  = 0b000000000000
funct12_EBREAK   :: MachineInt;    funct12_EBREAK = 0b000000000001
funct12_URET     :: MachineInt;    funct12_URET   = 0b000000000010
funct12_SRET     :: MachineInt;    funct12_SRET   = 0b000100000010
funct12_MRET     :: MachineInt;    funct12_MRET   = 0b001100000010
funct12_WFI      :: MachineInt;    funct12_WFI    = 0b000100000101

funct7_SFENCE_VMA :: MachineInt;    funct7_SFENCE_VMA = 0b0001001

funct3_CSRRW  :: MachineInt;    funct3_CSRRW  = 0b001
funct3_CSRRS  :: MachineInt;    funct3_CSRRS  = 0b010
funct3_CSRRC  :: MachineInt;    funct3_CSRRC  = 0b011
funct3_CSRRWI :: MachineInt;    funct3_CSRRWI = 0b101
funct3_CSRRSI :: MachineInt;    funct3_CSRRSI = 0b110
funct3_CSRRCI :: MachineInt;    funct3_CSRRCI = 0b111

-- AMO sub-opcodes, A standard extension
funct3_AMOW :: MachineInt;    funct3_AMOW = 0b010
funct3_AMOD :: MachineInt;    funct3_AMOD = 0b011

funct5_LR      :: MachineInt;    funct5_LR      = 0b00010
funct5_SC      :: MachineInt;    funct5_SC      = 0b00011
funct5_AMOSWAP :: MachineInt;    funct5_AMOSWAP = 0b00001
funct5_AMOADD  :: MachineInt;    funct5_AMOADD  = 0b00000
funct5_AMOXOR  :: MachineInt;    funct5_AMOXOR  = 0b00100
funct5_AMOAND  :: MachineInt;    funct5_AMOAND  = 0b01100
funct5_AMOOR   :: MachineInt;    funct5_AMOOR   = 0b01000
funct5_AMOMIN  :: MachineInt;    funct5_AMOMIN  = 0b10000
funct5_AMOMAX  :: MachineInt;    funct5_AMOMAX  = 0b10100
funct5_AMOMINU :: MachineInt;    funct5_AMOMINU = 0b11000
funct5_AMOMAXU :: MachineInt;    funct5_AMOMAXU = 0b11100

-- LOAD_FP sub-opcodes, F standard extension
funct3_FLW = 0b010 :: MachineInt

-- STORE_FP sub-opcodes, F standard extension
funct3_FSW = 0b010 :: MachineInt

-- OP_FP sub-opcodes, F standard extension
funct7_FADD_S = 0b0000000 :: MachineInt
funct7_FSUB_S = 0b0000100 :: MachineInt
funct7_FMUL_S = 0b0001000 :: MachineInt
funct7_FDIV_S = 0b0001100 :: MachineInt
funct7_FSQRT_S = 0b0101100 :: MachineInt
funct7_FSGNJ_S = 0b0010000 :: MachineInt
funct7_FMIN_S = 0b0010100 :: MachineInt
funct7_FCVT_W_S = 0b1100000 :: MachineInt
funct7_FMV_X_W = 0b1110000 :: MachineInt
funct7_FEQ_S = 0b1010000 :: MachineInt
funct7_FCLASS_S = 0b1110000 :: MachineInt
funct7_FCVT_S_W = 0b1101000 :: MachineInt
funct7_FMV_W_X = 0b1111000 :: MachineInt

funct3_FSGNJ_S = 0b000 :: MachineInt
funct3_FSGNJN_S = 0b001 :: MachineInt
funct3_FSGNJX_S = 0b010 :: MachineInt
funct3_FMIN_S = 0b000 :: MachineInt
funct3_FMAX_S = 0b001 :: MachineInt
funct3_FMV_X_W = 0b000 :: MachineInt
funct3_FEQ_S = 0b010 :: MachineInt
funct3_FLT_S = 0b001 :: MachineInt
funct3_FLE_S = 0b000 :: MachineInt
funct3_FCLASS_S = 0b001 :: MachineInt

rs2_FCVT_W_S = 0b00000 :: MachineInt
rs2_FCVT_WU_S = 0b00001 :: MachineInt
rs2_FCVT_L_S = 0b00010 :: MachineInt
rs2_FCVT_LU_S = 0b00011 :: MachineInt

-- MADD, MSUB, NMSUB, NMADD sub-opcodes, F standard extension
funct2_FMADD_S = 0b00

-- ================================================================
-- Identification of valid instructions in extensions

isValidI inst = inst /= InvalidI
isValidI64 inst = inst /= InvalidI64
isValidM inst = inst /= InvalidM
isValidM64 inst = inst /= InvalidM64
isValidA inst = inst /= InvalidA
isValidA64 inst = inst /= InvalidA64
isValidF inst = inst /= InvalidF
isValidF64 inst = inst /= InvalidF64
isValidCSR inst = inst /= InvalidCSR

head_default :: [ a ] -> a -> a
head_default [] v = v
head_default (h:_) _ = h

isAmbiguous :: [ a ] -> Bool
isAmbiguous [] = False
isAmbiguous [h] = False
isAmbiguous _ = True

-- ================================================================
-- The main decoder function

decode :: InstructionSet -> MachineInt -> Instruction
decode iset inst =
 if isAmbiguous results
  then error "ambiguous decoding result"
  else head_default results (InvalidInstruction inst)
  where
    results :: [Instruction]
    results =
      resultI ++
      (if supportsM iset then resultM else []) ++
      (if supportsA iset then resultA else []) ++
      (if supportsF iset then resultF else []) ++
      (if bitwidth iset == 64 then resultI64 else []) ++
      (if bitwidth iset == 64 && supportsM iset then resultM64 else []) ++
      (if bitwidth iset == 64 && supportsA iset then resultA64 else []) ++
      (if bitwidth iset == 64 && supportsF iset then resultF64 else []) ++
      resultCSR

    resultI = if isValidI decodeI then [IInstruction decodeI] else []
    resultM = if isValidM decodeM then [MInstruction decodeM] else []
    resultA = if isValidA decodeA then [AInstruction decodeA] else []
    resultF = if isValidF decodeF then [FInstruction decodeF] else []
    resultI64 = if isValidI64 decodeI64 then [I64Instruction decodeI64] else []
    resultM64 = if isValidM64 decodeM64 then [M64Instruction decodeM64] else []
    resultA64 = if isValidA64 decodeA64 then [A64Instruction decodeA64] else []
    resultF64 = if isValidF64 decodeF64 then [F64Instruction decodeF64] else []
    resultCSR = if isValidCSR decodeCSR then [CSRInstruction decodeCSR] else []

    -- Symbolic names for notable bitfields in the 32b instruction 'inst'
    -- Note: 'bitSlice x i j' is, roughly, Verilog's 'x [j-1, i]'
    opcode  = bitSlice inst 0 7        -- = Verilog's: inst [6:0]
    funct3  = bitSlice inst 12 15
    funct7  = bitSlice inst 25 32
    funct10 = (shift (bitSlice inst 25 32) 3) .|. (bitSlice inst 12 15)
    funct12 = bitSlice inst 20 32

    rd      = bitSlice inst 7 12
    rs1     = bitSlice inst 15 20
    rs2     = bitSlice inst 20 25
    rs3     = bitSlice inst 27 32    -- for FMADD, FMSUB, FNMSUB
    funct2  = bitSlice inst 25 27    -- for FMADD, FMSUB, FNMSUB
    rm      = funct3                 -- for FMADD, FMSUB, FNMSUB, and many OP_FPs.

    succ    = bitSlice inst 20 24    -- for FENCE
    pred    = bitSlice inst 24 28    -- for FENCE
    msb4    = bitSlice inst 28 32    -- for FENCE

    imm20   = signExtend 32 $ shift (bitSlice inst 12 32) 12    -- for LUI
    oimm20  = signExtend 32 $ shift (bitSlice inst 12 32) 12    -- for AUIPC

    jimm20  = signExtend 21 $ (shift (bitSlice inst 31 32) 20  .|.        -- for JAL
                                shift (bitSlice inst 21 31) 1  .|.
                                shift (bitSlice inst 20 21) 11 .|.
                                shift (bitSlice inst 12 20) 12)

    imm12   = signExtend 12 $ bitSlice inst 20 32
    oimm12  = signExtend 12 $ bitSlice inst 20 32

    csr12   = bitSlice inst 20 32

    simm12  = signExtend 12 $ shift (bitSlice inst 25 32) 5 .|. bitSlice inst 7 12    -- for STORE

    sbimm12 = signExtend 13 $ (shift (bitSlice inst 31 32) 12 .|.    -- for BRANCH
                                shift (bitSlice inst 25 31) 5 .|.
                                shift (bitSlice inst 8 12) 1  .|.
                                shift (bitSlice inst 7 8) 11)

    shamt5  = machineIntToShamt (bitSlice inst 20 25)
    shamt6  = machineIntToShamt (bitSlice inst 20 26)
    shamtHi = bitSlice inst 25 26
    funct6  = bitSlice inst 26 32
    shamtHiTest = shamtHi == 0 || bitwidth iset == 64

    zimm    = bitSlice inst 15 20    -- for CSRRxI

    funct5  = bitSlice inst 27 32    -- for A extension
    aqrl    = bitSlice inst 25 27    -- for A extension

    decodeI
      | opcode==opcode_LOAD, funct3==funct3_LB  = Lb  rd rs1 oimm12
      | opcode==opcode_LOAD, funct3==funct3_LH  = Lh  rd rs1 oimm12
      | opcode==opcode_LOAD, funct3==funct3_LW  = Lw  rd rs1 oimm12
      | opcode==opcode_LOAD, funct3==funct3_LBU = Lbu rd rs1 oimm12
      | opcode==opcode_LOAD, funct3==funct3_LHU = Lhu rd rs1 oimm12

      | opcode==opcode_MISC_MEM, rd==0, funct3==funct3_FENCE,   rs1==0, msb4==0  = Fence pred succ
      | opcode==opcode_MISC_MEM, rd==0, funct3==funct3_FENCE_I, rs1==0, imm12==0 = Fence_i

      | opcode==opcode_OP_IMM, funct3==funct3_ADDI  = Addi  rd rs1 imm12
      | opcode==opcode_OP_IMM, funct3==funct3_SLTI  = Slti  rd rs1 imm12
      | opcode==opcode_OP_IMM, funct3==funct3_SLTIU = Sltiu rd rs1 imm12
      | opcode==opcode_OP_IMM, funct3==funct3_XORI  = Xori  rd rs1 imm12
      | opcode==opcode_OP_IMM, funct3==funct3_ORI   = Ori   rd rs1 imm12
      | opcode==opcode_OP_IMM, funct3==funct3_ANDI  = Andi  rd rs1 imm12

      | opcode==opcode_OP_IMM, funct3==funct3_SLLI, funct6==funct6_SLLI, shamtHiTest = Slli rd rs1 shamt6
      | opcode==opcode_OP_IMM, funct3==funct3_SRLI, funct6==funct6_SRLI, shamtHiTest = Srli rd rs1 shamt6
      | opcode==opcode_OP_IMM, funct3==funct3_SRAI, funct6==funct6_SRAI, shamtHiTest = Srai rd rs1 shamt6

      | opcode==opcode_AUIPC = Auipc rd oimm20

      | opcode==opcode_STORE, funct3==funct3_SB = Sb rs1 rs2 simm12
      | opcode==opcode_STORE, funct3==funct3_SH = Sh rs1 rs2 simm12
      | opcode==opcode_STORE, funct3==funct3_SW = Sw rs1 rs2 simm12

      | opcode==opcode_OP, funct3==funct3_ADD,  funct7==funct7_ADD  = Add  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SUB,  funct7==funct7_SUB  = Sub  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SLL,  funct7==funct7_SLL  = Sll  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SLT,  funct7==funct7_SLT  = Slt  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SLTU, funct7==funct7_SLTU = Sltu rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_XOR,  funct7==funct7_XOR  = Xor  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SRL,  funct7==funct7_SRL  = Srl  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_SRA,  funct7==funct7_SRA  = Sra  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_OR,   funct7==funct7_OR   = Or   rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_AND,  funct7==funct7_AND  = And  rd rs1 rs2

      | opcode==opcode_LUI = Lui rd imm20

      | opcode==opcode_BRANCH, funct3==funct3_BEQ  = Beq  rs1 rs2 sbimm12
      | opcode==opcode_BRANCH, funct3==funct3_BNE  = Bne  rs1 rs2 sbimm12
      | opcode==opcode_BRANCH, funct3==funct3_BLT  = Blt  rs1 rs2 sbimm12
      | opcode==opcode_BRANCH, funct3==funct3_BGE  = Bge  rs1 rs2 sbimm12
      | opcode==opcode_BRANCH, funct3==funct3_BLTU = Bltu rs1 rs2 sbimm12
      | opcode==opcode_BRANCH, funct3==funct3_BGEU = Bgeu rs1 rs2 sbimm12

      | opcode==opcode_JALR, funct3==funct3_JALR = Jalr rd rs1 oimm12
      | opcode==opcode_JAL = Jal rd jimm20

      | True = InvalidI

    decodeM
      | opcode==opcode_OP, funct3==funct3_MUL,    funct7==funct7_MUL    = Mul    rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_MULH,   funct7==funct7_MULH   = Mulh   rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_MULHSU, funct7==funct7_MULHSU = Mulhsu rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_MULHU,  funct7==funct7_MULHU  = Mulhu  rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_DIV,    funct7==funct7_DIV    = Div    rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_DIVU,   funct7==funct7_DIVU   = Divu   rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_REM,    funct7==funct7_REM    = Rem    rd rs1 rs2
      | opcode==opcode_OP, funct3==funct3_REMU,   funct7==funct7_REMU   = Remu   rd rs1 rs2
      | True = InvalidM

    decodeA
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_LR, rs2==0 = Lr_w      rd rs1 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_SC         = Sc_w      rd rs1 rs2 aqrl

      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOSWAP    = Amoswap_w rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOADD     = Amoadd_w  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOXOR     = Amoxor_w  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOAND     = Amoand_w  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOOR      = Amoor_w   rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOMIN     = Amomin_w  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOMAX     = Amomax_w  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOMINU    = Amominu_w rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOW, funct5==funct5_AMOMAXU    = Amomaxu_w rd rs1 rs2 aqrl

      | True = InvalidA

    decodeF
      | opcode==opcode_LOAD_FP, funct3==funct3_FLW = Flw rd rs1 oimm12
      | opcode==opcode_STORE_FP, funct3==funct3_FSW = Fsw rs1 rs2 simm12
      | opcode==opcode_MADD, funct2==funct2_FMADD_S = Fmadd_s rd rs1 rs2 rs3 rm
      | opcode==opcode_MSUB, funct2==funct2_FMADD_S = Fmsub_s rd rs1 rs2 rs3 rm
      | opcode==opcode_NMSUB, funct2==funct2_FMADD_S = Fnmsub_s rd rs1 rs2 rs3 rm
      | opcode==opcode_NMADD, funct2==funct2_FMADD_S = Fnmadd_s rd rs1 rs2 rs3 rm

      | opcode==opcode_OP_FP, funct7==funct7_FADD_S = Fadd_s rd rs1 rs2 rm
      | opcode==opcode_OP_FP, funct7==funct7_FSUB_S = Fsub_s rd rs1 rs2 rm
      | opcode==opcode_OP_FP, funct7==funct7_FMUL_S = Fmul_s rd rs1 rs2 rm
      | opcode==opcode_OP_FP, funct7==funct7_FDIV_S = Fdiv_s rd rs1 rs2 rm
      | opcode==opcode_OP_FP, funct7==funct7_FSQRT_S, rs2==0 = Fsqrt_s rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FSGNJ_S, funct3==funct3_FSGNJ_S = Fsgnj_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FSGNJ_S, funct3==funct3_FSGNJN_S = Fsgnjn_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FSGNJ_S, funct3==funct3_FSGNJX_S = Fsgnjx_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FMIN_S, funct3==funct3_FMIN_S = Fmin_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FMIN_S, funct3==funct3_FMAX_S = Fmax_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_W_S, rs2==rs2_FCVT_W_S = Fcvt_w_s rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_W_S, rs2==rs2_FCVT_WU_S = Fcvt_wu_s rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FMV_X_W, rs2==0, funct3==0 = Fmv_x_w rd rs1
      | opcode==opcode_OP_FP, funct7==funct7_FEQ_S, funct3==funct3_FEQ_S = Feq_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FEQ_S, funct3==funct3_FLT_S = Flt_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FEQ_S, funct3==funct3_FLE_S = Fle_s rd rs1 rs2
      | opcode==opcode_OP_FP, funct7==funct7_FCLASS_S, rs2==0, funct3==funct3_FCLASS_S = Fclass_s rd rs1
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_S_W, rs2==rs2_FCVT_W_S = Fcvt_s_w rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_S_W, rs2==rs2_FCVT_WU_S = Fcvt_s_wu rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FMV_W_X, rs2==0, funct3==0 = Fmv_w_x rd rs1
      | True = InvalidF

    decodeI64
      | opcode==opcode_LOAD, funct3==funct3_LD  = Ld  rd rs1 oimm12
      | opcode==opcode_LOAD, funct3==funct3_LWU = Lwu rd rs1 oimm12

      | opcode==opcode_OP_IMM_32, funct3==funct3_ADDIW                       = Addiw rd rs1 imm12
      | opcode==opcode_OP_IMM_32, funct3==funct3_SLLIW, funct7==funct7_SLLIW = Slliw rd rs1 shamt5
      | opcode==opcode_OP_IMM_32, funct3==funct3_SRLIW, funct7==funct7_SRLIW = Srliw rd rs1 shamt5
      | opcode==opcode_OP_IMM_32, funct3==funct3_SRAIW, funct7==funct7_SRAIW = Sraiw rd rs1 shamt5

      | opcode==opcode_STORE, funct3==funct3_SD = Sd rs1 rs2 simm12

      | opcode==opcode_OP_32, funct3==funct3_ADDW, funct7==funct7_ADDW = Addw rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_SUBW, funct7==funct7_SUBW = Subw rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_SLLW, funct7==funct7_SLLW = Sllw rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_SRLW, funct7==funct7_SRLW = Srlw rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_SRAW, funct7==funct7_SRAW = Sraw rd rs1 rs2

      | True = InvalidI64

    decodeM64
      | opcode==opcode_OP_32, funct3==funct3_MULW,  funct7==funct7_MULW  = Mulw  rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_DIVW,  funct7==funct7_DIVW  = Divw  rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_DIVUW, funct7==funct7_DIVUW = Divuw rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_REMW,  funct7==funct7_REMW  = Remw  rd rs1 rs2
      | opcode==opcode_OP_32, funct3==funct3_REMUW, funct7==funct7_REMUW = Remuw rd rs1 rs2
      | True = InvalidM64

    decodeA64
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_LR, rs2==0 = Lr_d      rd rs1 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_SC         = Sc_d      rd rs1 rs2 aqrl

      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOSWAP    = Amoswap_d rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOADD     = Amoadd_d  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOXOR     = Amoxor_d  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOAND     = Amoand_d  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOOR      = Amoor_d   rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOMIN     = Amomin_d  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOMAX     = Amomax_d  rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOMINU    = Amominu_d rd rs1 rs2 aqrl
      | opcode==opcode_AMO, funct3==funct3_AMOD, funct5==funct5_AMOMAXU    = Amomaxu_d rd rs1 rs2 aqrl

      | True = InvalidA64

    decodeF64
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_W_S, rs2==rs2_FCVT_L_S = Fcvt_l_s rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_W_S, rs2==rs2_FCVT_LU_S = Fcvt_lu_s rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_S_W, rs2==rs2_FCVT_L_S = Fcvt_s_l rd rs1 rm
      | opcode==opcode_OP_FP, funct7==funct7_FCVT_S_W, rs2==rs2_FCVT_LU_S = Fcvt_s_lu rd rs1 rm
      | True = InvalidF64

    decodeCSR
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, funct7==funct7_SFENCE_VMA        = Sfence_vma rs1 rs2
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_ECALL  = Ecall
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_EBREAK = Ebreak
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_URET   = Uret
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_SRET   = Sret
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_MRET   = Mret
      | opcode==opcode_SYSTEM, rd==0, funct3==funct3_PRIV, rs1==0, funct12==funct12_WFI    = Wfi
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRW  = Csrrw rd rs1 csr12
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRS  = Csrrs rd rs1 csr12
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRC  = Csrrc rd rs1 csr12
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRWI = Csrrwi rd zimm csr12
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRSI = Csrrsi rd zimm csr12
      | opcode==opcode_SYSTEM, funct3==funct3_CSRRCI = Csrrci rd zimm csr12
      | True = InvalidCSR

-- ================================================================
