{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Execute where
import Decode
import Data.Bits
import Data.Int
import Data.Word

-- Utility function.
setIndex :: Int -> a -> [a] -> [a]
setIndex i x l = left ++ (x:(drop 1 right))
  where (left, right) = splitAt i l

type Memory = (Int, [Int32])

createMemory :: Int -> Memory
createMemory size = (size, take size $ cycle [0])

getSize :: Memory -> Int
getSize (size, contents) = size

getAddress :: Int32 -> Memory -> Int32
getAddress addr (size, content)
  | addr >= 0 && iaddr < size = content !! iaddr
  | otherwise = error "Invalid address."
  where iaddr = fromIntegral addr

setAddress :: Int32 -> Int32 -> Memory -> Memory
setAddress addr val (size, content)
  | addr >= 0 && iaddr < size = (size, setIndex iaddr val content)
  | otherwise = error "Invalid address."
  where iaddr = fromIntegral addr

data Processor = Processor { registers :: [Int32], pc :: Int32 }
               deriving (Read, Show)

getRegister :: Register -> Processor -> Int32
getRegister reg cpu | reg == 0  = 0 -- always return 0 from register 0
                    | otherwise = (registers cpu) !! ((fromIntegral reg :: Int)-1)

setRegister :: Register -> Int32 -> Processor -> Processor
setRegister reg val cpu | reg == 0  = cpu -- do nothing if register 0 is selected.
                        | otherwise = cpu { registers = setIndex ((fromIntegral reg :: Int)-1) val (registers cpu) }

getPC :: Processor -> Int32
getPC = pc

setPC :: Int32 -> Processor -> Processor
setPC val cpu = cpu { pc = val }

unsigned :: Int32 -> Word32
unsigned = fromIntegral
signed :: Word32 -> Int32
signed = fromIntegral

s8 :: Int32 -> Int32
s8 n = fromIntegral (fromIntegral n :: Int8) :: Int32

s16 :: Int32 -> Int32
s16 n = fromIntegral (fromIntegral n :: Int16) :: Int32

u8 :: Int32 -> Int32
u8 n = fromIntegral (fromIntegral n :: Word8) :: Int32

u16 :: Int32 -> Int32
u16 n = fromIntegral (fromIntegral n :: Word16) :: Int32

lower5 :: Int32 -> Int
lower5 x = fromIntegral $ bitSlice x 0 5

class (Num t, Monad p) => RiscvProgram p t | p -> t where
   readRegister :: Int32 -> p t
   writeRegister :: Int32 -> t -> p ()
   load :: t -> p t
   store :: t -> t -> p ()

-- execute' :: RiscvProgram p t => Instruction -> p ()
execute' (Add rd rs1 rs2) = do
   a1 <- readRegister rs1
   a2 <- readRegister rs2
   writeRegister rd (a1+a2)

execute :: (Memory, Processor) -> Instruction -> (Memory, Processor)
execute (mem, cpu) (Lui rd imm20) = (mem, setRegister rd imm20 cpu)
execute (mem, cpu) (Auipc rd imm20) = (mem, setRegister rd (imm20 + getPC cpu) cpu)
execute (mem, cpu) (Jal rd jimm20) = (mem, setPC (getPC cpu + jimm20) $ setRegister rd (getPC cpu + 4) cpu)
execute (mem, cpu) (Jalr rd rs1 oimm12) = (mem, setPC (getRegister rs1 cpu + oimm12) $ setRegister rd (getPC cpu + 4) cpu)
execute (mem, cpu) (Beq rs1 rs2 sbimm12) = (mem, if getRegister rs1 cpu == getRegister rs2 cpu then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Bne rs1 rs2 sbimm12) = (mem, if getRegister rs1 cpu /= getRegister rs2 cpu then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Blt rs1 rs2 sbimm12) = (mem, if getRegister rs1 cpu < getRegister rs2 cpu then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Bge rs1 rs2 sbimm12) = (mem, if getRegister rs1 cpu < getRegister rs2 cpu then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Bltu rs1 rs2 sbimm12) = (mem, if (unsigned $ getRegister rs1 cpu) < (unsigned $ getRegister rs2 cpu) then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Bgeu rs1 rs2 sbimm12) = (mem, if (unsigned $ getRegister rs1 cpu) < (unsigned $ getRegister rs2 cpu) then setPC sbimm12 cpu else cpu)
execute (mem, cpu) (Lb rd rs1 oimm12) = (mem, setRegister rd (s8 $ getAddress (getRegister rs1 cpu + oimm12) mem) cpu)
execute (mem, cpu) (Lh rd rs1 oimm12) = (mem, setRegister rd (s16 $ getAddress (getRegister rs1 cpu + oimm12) mem) cpu)
execute (mem, cpu) (Lw rd rs1 oimm12) = (mem, setRegister rd (getAddress (getRegister rs1 cpu + oimm12) mem) cpu)
execute (mem, cpu) (Lbu rd rs1 oimm12) = (mem, setRegister rd (u8 $ getAddress (getRegister rs1 cpu + oimm12) mem) cpu)
execute (mem, cpu) (Lhu rd rs1 oimm12) = (mem, setRegister rd (u8 $ getAddress (getRegister rs1 cpu + oimm12) mem) cpu)
execute (mem, cpu) (Sb rs1 rs2 simm12) = (setAddress (getRegister rs1 cpu + simm12) (s8 $ getRegister rs2 cpu) mem, cpu)
execute (mem, cpu) (Sh rs1 rs2 simm12) = (setAddress (getRegister rs1 cpu + simm12) (s16 $ getRegister rs2 cpu) mem, cpu)
execute (mem, cpu) (Sw rs1 rs2 simm12) = (setAddress (getRegister rs1 cpu + simm12) (getRegister rs2 cpu) mem, cpu)
execute (mem, cpu) (Addi rd rs1 imm12) = (mem, setRegister rd (getRegister rs1 cpu + imm12) cpu)
execute (mem, cpu) (Slti rd rs1 imm12) = (mem, setRegister rd (if getRegister rs1 cpu < imm12 then 1 else 0) cpu)
execute (mem, cpu) (Sltiu rd rs1 imm12) = (mem, setRegister rd (if (unsigned $ getRegister rs1 cpu) < (unsigned $ imm12) then 1 else 0) cpu)
execute (mem, cpu) (Xori rd rs1 imm12) = (mem, setRegister rd (xor (getRegister rs1 cpu) imm12) cpu)
execute (mem, cpu) (Ori rd rs1 imm12) = (mem, setRegister rd ((.|.) (getRegister rs1 cpu) imm12) cpu)
execute (mem, cpu) (Andi rd rs1 imm12) = (mem, setRegister rd ((.&.) (getRegister rs1 cpu) imm12) cpu)
execute (mem, cpu) (Slli rd rs1 imm12) = (mem, setRegister rd (shiftL (getRegister rs1 cpu) (lower5 imm12)) cpu)
execute (mem, cpu) (Srli rd rs1 imm12) = (mem, setRegister rd (signed $ shiftR (unsigned $ getRegister rs1 cpu) (lower5 imm12)) cpu)
execute (mem, cpu) (Srai rd rs1 imm12) = (mem, setRegister rd (shiftR (getRegister rs1 cpu) (lower5 imm12)) cpu)
execute (mem, cpu) (Add rd rs1 rs2) = (mem, setRegister rd (getRegister rs1 cpu + getRegister rs2 cpu) cpu)
execute (mem, cpu) (Sub rd rs1 rs2) = (mem, setRegister rd (getRegister rs1 cpu - getRegister rs2 cpu) cpu)
execute (mem, cpu) (Sll rd rs1 rs2) = (mem, setRegister rd (shiftL (getRegister rs1 cpu) (lower5 $ getRegister rs2 cpu)) cpu)
execute (mem, cpu) (Slt rd rs1 rs2) = (mem, setRegister rd (if getRegister rs1 cpu < getRegister rs2 cpu then 1 else 0) cpu)
execute (mem, cpu) (Sltu rd rs1 rs2) = (mem, setRegister rd (if (unsigned $ getRegister rs1 cpu) < (unsigned $ getRegister rs2 cpu) then 1 else 0) cpu)
execute (mem, cpu) (Xor rd rs1 rs2) = (mem, setRegister rd (xor (getRegister rs1 cpu) (getRegister rs2 cpu)) cpu)
execute (mem, cpu) (Or rd rs1 rs2) = (mem, setRegister rd ((.|.) (getRegister rs1 cpu) (getRegister rs2 cpu)) cpu)
execute (mem, cpu) (Srl rd rs1 rs2) = (mem, setRegister rd (signed $ shiftR (unsigned $ getRegister rs1 cpu) (lower5 $ getRegister rs2 cpu)) cpu)
execute (mem, cpu) (Sra rd rs1 rs2) = (mem, setRegister rd (shiftR (getRegister rs1 cpu) (lower5 $ getRegister rs2 cpu)) cpu)
execute (mem, cpu) (And rd rs1 rs2) = (mem, setRegister rd ((.&.) (getRegister rs1 cpu) (getRegister rs2 cpu)) cpu)
-- TODO: Fence/Fence.i?

mem = createMemory 10
cpu = Processor (take 8 $ cycle [0]) 78
