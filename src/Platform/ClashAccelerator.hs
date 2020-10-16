{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}
module Platform.ClashAccelerator where
--import qualified Prelude as P
import Spec.Machine
import Utility.Utility
import Spec.CSR
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
import qualified Data.Map as S
import Spec.ExecuteClash
import qualified Decode as D
import Clash.Prelude
import qualified Spec.Memory as M
import Utility.MapMemory()
import GcdExpr

data MMIOClash = MMIOClash {  registers :: Vec 31 Int32, pc :: Int32, nextPC :: Int32 , store:: Maybe (Int32,Int32,(Bool,Bool,Bool,Bool)), gcdI1 :: Int32, gcdI2 :: Int32 , exception :: Bool }
              deriving (Show) -- Beurk, I need to use Word32 instead of Int32 for the addresses.

type MState = State MMIOClash
type MMState = MaybeT MState

instance RiscvProgram MState Int32 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp ->((), if reg == 0 then comp else comp { registers = replace (fromIntegral reg-1) (fromIntegral val) (registers comp) })
-- Fake load and stores
  loadWord a = state $ \comp -> (
                        if (a == 0x4000) 
                         then gcdI1 comp
                         else if (a == 0x4004)
                           then gcdI2 comp
                           else 
                            0                   
     ,comp)
  storeWord a v = state $ \comp -> ((), comp{store = Just (fromIntegral a, fromIntegral v,(True,True,True,True))})
  getCSRField field = state $ \comp -> (0, comp)
  setCSRField field val = state $ \comp -> ((), comp)
  getPC = state $ \comp -> (pc comp, comp)
  setPC val = state $ \comp -> ((), comp { nextPC = fromIntegral val})
  commit = state $ \comp -> ((), comp { pc = nextPC comp })
  getPrivMode = state $ \comp -> (Machine, comp) 
  setPrivMode m = state $ \comp -> ((), comp)

-- GCD program 
oneStep :: Int32 ->  MState ()
oneStep i = do 
  result <- runMaybeT $ do
    pc <- getPC
    setPC (pc + 4)
    let a = D.decode D.RV32I $ fromIntegral i
    execute a
  case result of
    Nothing -> commit >> (state $ \comp -> ((), comp{exception = True})) -- early return
    Just r -> commit >> return r -- this is a ()

wrap :: Int32 -> MMIOClash-> MMIOClash
wrap i s = snd $ runState (oneStep i) s

topEntity:: Clock System Source -> Reset System Asynchronous ->
    Signal System (Int32,Int32) -> Signal System (Int32,Int32) 
topEntity = exposeClockReset machine
machine = mealy 
     (\(iregister::Vec 31 Int32,pc::Int32) 
                     (gcd1::Int32, gcd2::Int32) ->
                      let i = gcdState pc 
                      in 
                      let newstate = wrap i MMIOClash{registers = reverse iregister,
                                                      store = Nothing,
                                                      pc = pc,
                                                      gcdI1 = gcd1,
                                                      gcdI2 = gcd2,
                                                      exception = False
                                                     }
                      in
                        let storeNext = store newstate
                            npc = nextPC newstate
                        in
                          ((reverse $ registers newstate, npc ), (

--                          ((iregister, pc ), (
                           (\(x,y,z)->x) $ fromMaybe (0,0,(False,False,False,False)) storeNext,
                           (\(x,y,z)->y) $ fromMaybe (0,0,(False,False,False,False)) storeNext
                           ))) (replicate (SNat :: SNat 31) 0, 0)
