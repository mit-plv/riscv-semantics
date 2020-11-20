{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, Rank2Types #-}
module Spec.Machine where
import Spec.CSRField
import qualified Spec.CSR as CSR
import Spec.Decode
import Utility.Utility
import Data.Int
import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Prelude
import Debug.Trace

-- Note that this is ordered: User < Supervisor < Machine
data PrivMode = User | Supervisor | Machine deriving (Eq, Ord, Show)
data AccessType = Instruction | Load | Store deriving (Eq, Show)

data SourceType = VirtualMemory | Fetch | Execute deriving (Eq,Show)

decodePrivMode 0 = User
decodePrivMode 1 = Supervisor
decodePrivMode 3 = Machine
decodePrivMode _ = error "Invalid privilege mode"

encodePrivMode User = 0
encodePrivMode Supervisor = 1
encodePrivMode Machine = 3

data Platform = Platform {
  -- The existential on p and t is per-function, rather than a type variable of
  -- Platform, both for semantic clarity and because Having it as a type
  -- variable of Platform would break lifting.
  dirtyHardware :: forall p t. (RiscvMachine p t) => p Bool,
  readPlatformCSR :: forall p t. (RiscvMachine p t) => CSR.CSR -> p MachineInt,
  writePlatformCSRField :: forall p t. (RiscvMachine p t) => CSRField -> MachineInt -> p MachineInt
  -- writeMISA :: forall p t. (RiscvMachine p t) => MachineInt -> p MachineInt
}

class (Monad p, MachineWidth t) => RiscvMachine p t | p -> t where
  getRegister :: Register -> p t
  setRegister :: Register -> t -> p ()
  -- TODO: Another typeclass parameter for floating-point width?
  getFPRegister :: FPRegister -> p Int32
  setFPRegister :: FPRegister -> Int32 -> p ()
  loadByte :: SourceType -> t -> p Int8
  loadHalf :: SourceType -> t -> p Int16
  loadWord :: SourceType -> t -> p Int32
  loadDouble :: SourceType -> t -> p Int64
  storeByte :: SourceType -> t -> Int8 -> p ()
  storeHalf :: SourceType -> t -> Int16 -> p ()
  storeWord :: SourceType -> t -> Int32 -> p ()
  storeDouble :: SourceType -> t -> Int64 -> p ()
  makeReservation :: t -> p ()
  checkReservation :: t -> p Bool
  clearReservation :: t -> p ()
  getCSRField :: CSRField -> p MachineInt
  unsafeSetCSRField :: (Integral s) => CSRField -> s -> p ()
  getPC :: p t
  setPC :: t -> p ()
  getPrivMode :: p PrivMode
  setPrivMode :: PrivMode -> p ()
  commit :: p ()
  endCycle :: forall z. p z
  flushTLB :: p ()
  fence :: MachineInt -> MachineInt -> p ()
  getPlatform :: p Platform


cacheAccess :: forall p t. (RiscvMachine p t) => AccessType -> MachineInt -> p (MachineInt, MachineInt,  Int) -> p MachineInt
cacheAccess accessType addr getPA = do --remove TLB tracking, the only people that model stalled TLB entries seem to be doing it in an axiomatic memory model fashion
      (pa, _pte, _level) <- getPA
      return pa

getXLEN :: forall p t s. (RiscvMachine p t, Integral s) => p s
getXLEN = do
            mxl <- getCSRField MXL
            case mxl of
                1 -> return 32
                2 -> return 64

hardwareDirtyBit :: (RiscvMachine p t) => p Bool
hardwareDirtyBit = do
  p <- getPlatform
  r <- dirtyHardware p
  return r

getCSR_InstRet :: (RiscvMachine p t) => p MachineInt
getCSR_InstRet = do
  p <- getPlatform
  r <- readPlatformCSR p CSR.InstRet
  return r

getCSR_Time :: (RiscvMachine p t) => p MachineInt
getCSR_Time = do
  p <- getPlatform
  readPlatformCSR p CSR.Time

getCSR_Cycle :: (RiscvMachine p t) => p MachineInt
getCSR_Cycle = do
  p <- getPlatform
  readPlatformCSR p CSR.Cycle

setCSRField :: (RiscvMachine p t, Integral s) => CSRField -> s -> p ()
setCSRField field value = do
  let ty = fieldType field
  if (ty == WLRL || ty == WARL) then do
    p <- getPlatform
    v <- (writePlatformCSRField p) field (fromIntegral value)
    unsafeSetCSRField field v
  else unsafeSetCSRField field value

instance (RiscvMachine p t) => RiscvMachine (MaybeT p) t where
  getRegister r = lift (getRegister r)
  setRegister r v = lift (setRegister r v)
  getFPRegister r = lift (getFPRegister r)
  setFPRegister r v = lift (setFPRegister r v)
  loadByte s a = lift (loadByte s a)
  loadHalf s a = lift (loadHalf s a)
  loadWord s a = lift (loadWord s a)
  loadDouble s a = lift (loadDouble s a)
  storeByte s a v = lift (storeByte s a v)
  storeHalf s a v = lift (storeHalf s a v)
  storeWord s a v = lift (storeWord s a v)
  storeDouble s a v = lift (storeDouble s a v)
  makeReservation a = lift (makeReservation a)
  checkReservation a = lift (checkReservation a)
  clearReservation a = lift (clearReservation a)
  getCSRField f = lift (getCSRField f)
  unsafeSetCSRField f v = lift (unsafeSetCSRField f v)
  getPC = lift getPC
  setPC v = lift (setPC v)
  getPrivMode = lift getPrivMode
  setPrivMode m = lift (setPrivMode m)
  commit = lift commit
  endCycle = MaybeT (return Nothing) -- b is of type (MaybeT p) a
  flushTLB = lift flushTLB
  fence a b = lift (fence a b)
  getPlatform = lift getPlatform

raiseExceptionWithInfo :: forall a p t. (RiscvMachine p t) => MachineInt -> MachineInt -> MachineInt -> p a
raiseExceptionWithInfo isInterrupt exceptionCode info =  do
  pc <- getPC
  mode <- getPrivMode
  medeleg <- getCSRField MEDeleg
  mideleg <- getCSRField MIDeleg
  let delegatedException = isInterrupt == 0 && (testBit medeleg ((fromIntegral:: MachineInt -> Int) exceptionCode))
  let delegatedInterrupt = isInterrupt /= 0 && (testBit mideleg ((fromIntegral ::MachineInt -> Int) exceptionCode))
  if (mode < Machine) && (delegatedException || delegatedInterrupt)
    then do
    -- Delegate to S-mode.
    addr <- getCSRField STVecBase
    setPrivMode Supervisor
    setCSRField STVal info
    setCSRField SPP (encodePrivMode mode)
    setCSRField SEPC pc
    setCSRField SCauseInterrupt isInterrupt
    setCSRField SCauseCode exceptionCode
    sie <- getCSRField SIE
    setCSRField SPIE sie
    setCSRField SIE 0
    setPC ((fromIntegral:: MachineInt -> t) addr * 4)
    else do
    -- Handle in M-mode.
    addr <- getCSRField MTVecBase
    setPrivMode Machine
    setCSRField MTVal info
    setCSRField MPP (encodePrivMode mode)
    mie <- getCSRField MIE
    setCSRField MPIE mie
    setCSRField MIE 0
    setCSRField MEPC pc
    setCSRField MCauseInterrupt isInterrupt
    setCSRField MCauseCode exceptionCode
    setPC ((fromIntegral:: MachineInt -> t) addr * 4)
  endCycle

raiseException :: forall a p t. (RiscvMachine p t) => MachineInt -> MachineInt -> p a
raiseException isInterrupt exceptionCode = raiseExceptionWithInfo isInterrupt exceptionCode 0
