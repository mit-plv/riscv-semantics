{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module MMIOClash where
import qualified Prelude as P
import Program
import Utility
import CSR
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO.Error
import qualified Data.Map as S
import Execute32
import qualified Decode as D
import CLaSH.Prelude

newtype MState s a = MState { runState :: s -> Maybe (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> fmap (\(b,s) -> (f b, s)) (runState a state)

instance Applicative (MState s) where
  pure x = MState $ \state -> return (x, state)
  (<*>) f a = MState $ \state -> do
    (g, s1) <- runState f state
    (b, s2) <- runState a s1
    return (g b, s2)

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> runState a state >>= (\(b, s) -> runState (f b) s)

instance Alternative (MState s) where
  empty = MState $ \state -> empty
  (<|>) a b = MState $ \state -> runState a state <|> runState b state

instance MonadPlus (MState s)

data MMIOClash = MMIOClash { registers :: Vec 31 Int32, pc :: Int32, nextPC :: Int32, mem :: Vec 10000 Word8}
              deriving (Show)

-- open, close, read, write

readM mem addr = mem !! addr

writeM addr val mem =
    replace addr val mem

helpStore mem addr bytes =
    P.foldr (\(b,a) m-> writeM a b m) mem $ P.zip bytes [addr + i | i<-[0..]] 


instance RiscvProgram (MState MMIOClash) Int32 Word32 where
  getRegister reg = MState $ \comp -> return (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> return ((), if reg == 0 then comp else comp { registers = replace (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte = undefined
  loadHalf= undefined
  loadDouble= undefined
  storeByte = undefined
  storeHalf= undefined
  storeDouble = undefined
  loadCSR= undefined
  storeCSR=undefined
--  loadByte addr = MState $ \comp -> return (fromIntegral $ readM (mem comp) (fromIntegral addr), comp)
--  loadHalf addr = MState $ \comp -> return (combineBytes $ fmap (\addr -> readM (mem comp) addr) [(fromIntegral addr)..(fromIntegral addr+1)], comp)
  loadWord addr = MState $ \comp -> if
    | otherwise -> return (combineBytes $ fmap (\addr -> readM (mem comp) addr) [(fromIntegral addr),fromIntegral addr +1,fromIntegral addr +2,(fromIntegral addr+3)], comp)
--  storeByte addr val = MState $ \comp -> return ((), comp { mem = writeM (fromIntegral addr) (fromIntegral val) (mem comp) })
--  storeHalf addr val = MState $ \comp -> return ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitHalf val) })
  storeWord addr val = MState $ \comp -> if
    | otherwise -> return ((), comp { mem = helpStore (mem comp) (fromIntegral addr) (splitWord val) })
--  loadCSR addr = MState $ \comp -> return (0::MachineInt, comp)
--  storeCSR addr val = MState $ \comp -> return ((), comp)
  getPC = MState $ \comp -> return (pc comp, comp)
  setPC val = MState $ \comp -> return ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> return ((), comp { pc = nextPC comp })
--  loadDouble addr =  MState $ \comp -> return (0::Int64 , comp)
--  storeDouble addr val=  MState $ \comp ->return ((), comp )

oneStep :: MState MMIOClash ()
oneStep = do
  pc <- getPC
  inst <- loadWord pc
  setPC (pc + 4)
  execute (D.decode $ fromIntegral inst)
  step

wrap :: Maybe (MMIOClash)-> Maybe MMIOClash
wrap (Just s) = case runState oneStep s of
           Nothing -> Nothing
           Just (_,s) -> Just s

wrap Nothing = Nothing

initState = MMIOClash { registers = replicate (SNat :: SNat 31) 0, pc = 0x200, nextPC = 0,
                   mem =replicate (SNat :: SNat 10000) 0  }

topEntity :: Signal (Maybe MMIOClash) -> Signal (Maybe MMIOClash)
topEntity = moore (\s i ->wrap s) (\s -> s) $ Just initState
