{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module Clash where
--import qualified Prelude as P
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
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO.Error
import qualified Data.Map as S
import ExecuteClash
import qualified Decode as D
import Clash.Prelude


data MMIOClash = MMIOClash { registers :: Vec 31 Int32, pc :: Int32, nextPC :: Int32 , store:: Maybe (Int32,Int32), load :: Int32, loadAddress :: Int32 }
              deriving (Show)

type MState = State MMIOClash


instance RiscvProgram MState Int32 Word32 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp ->((), if reg == 0 then comp else comp { registers = replace (fromIntegral reg-1) (fromIntegral val) (registers comp) })
-- Fake load and stores
  loadByte a = state $ \comp -> (0, comp)
  loadHalf a =state $ \comp -> (0, comp)
  loadWord a = state $ \comp -> (load comp, comp{loadAddress = fromIntegral a})
  loadDouble a = state $ \comp -> (0, comp)
  storeByte a v = state $ \comp -> ((), comp)
  storeHalf a v = state $ \comp -> ((), comp)
  storeWord  a v = state $ \comp -> ((), comp{store=Just (fromIntegral a, fromIntegral v)})
  storeDouble  a v = state $ \comp -> ((), comp)
-- fake CSR
  getCSRField field = state $ \comp -> (0, comp)
  setCSRField field val = state $ \comp -> ((), comp)
  getPC = state $ \comp -> (pc comp, comp)
  setPC val = state $ \comp -> ((), comp { nextPC = fromIntegral val })
  step = state $ \comp -> ((), comp { pc = nextPC comp })

oneStep :: Int32 -> MState ()
oneStep i = do
  pc <- getPC
  setPC (pc + 4)
  execute (D.decode 32 $ fromIntegral i)
  step

wrap :: Int32 -> MMIOClash-> MMIOClash
wrap i s = snd $ runState (oneStep i) s


topEntity :: SystemClockReset
  => Signal System (Int32, MMIOClash)
  -> Signal System (MMIOClash)
topEntity = fmap (\(i,s) -> wrap i s )
