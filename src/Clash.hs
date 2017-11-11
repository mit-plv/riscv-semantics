{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module Clash where
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
import Clash.Prelude

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

data MMIOClash = MMIOClash { registers :: Vec 31 Int32, pc :: Int32, nextPC :: Int32 }
              deriving (Show)

-- open, close, read, write

-- readM mem addr = mem !! addr
-- 
-- writeM addr val mem =
--     replace addr val mem
-- 
-- helpStore mem addr bytes =
--     P.foldr (\(b,a) m-> writeM a b m) mem $ P.zip bytes [addr + i | i<-[0..]] 


instance RiscvProgram (MState MMIOClash) Int32 Word32 where
  getXLEN = MState $ \comp -> return (32, comp)
  getRegister reg = MState $ \comp -> return (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> return ((), if reg == 0 then comp else comp { registers = replace (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte = undefined
  loadHalf= undefined
  loadWord = undefined
  loadDouble= undefined
  storeByte = undefined
  storeHalf= undefined
  storeWord = undefined
  storeDouble = undefined
  getCSRField = undefined
  setCSRField = undefined
  getPC = MState $ \comp -> return (pc comp, comp)
  setPC val = MState $ \comp -> return ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> return ((), comp { pc = nextPC comp })

oneStep :: Int32 -> MState MMIOClash ()
oneStep i = do
  pc <- getPC
  setPC (pc + 4)
  size <- getXLEN
  execute (D.decode size $ fromIntegral i)
  step

wrap :: Int32 -> Maybe (MMIOClash)-> Maybe MMIOClash
wrap i (Just s) = case runState (oneStep i) s of
           Nothing -> Nothing
           Just (_,s) -> Just s
wrap i Nothing = Nothing

initState = MMIOClash { registers = replicate (SNat :: SNat 31) 0, pc = 0x200, nextPC = 0 }
--topEntity :: Sigonal (Maybe MMIOClash) -> Signal (())
topEntity :: SystemClockReset
  => Signal System Int32
  -> Signal System (Maybe MMIOClash)
topEntity = mealy (\s i -> (wrap i s,wrap i s)) $ Just initState
