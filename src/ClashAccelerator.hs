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
import qualified Memory as M
import MapMemory()


data MMIOClash = MMIOClash { registers :: Vec 31 Int32, pc :: Int32, nextPC :: Int32 , store:: Maybe (Int32,Int32,(Bool,Bool,Bool,Bool)), gcdI1 :: Int32, gcdI2 :: Int32 , exception :: Bool, program :: S.Map Int Word8 }
              deriving (Show) -- Beurk, I need to use Word32 instead of Int32 for the addresses.

type MState = State MMIOClash
type MMState = MaybeT MState

instance RiscvProgram MState Int32 where
  getRegister reg = state $ \comp -> (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = state $ \comp ->((), if reg == 0 then comp else comp { registers = replace (fromIntegral reg-1) (fromIntegral val) (registers comp) })
-- Fake load and stores
  loadWord a = state $ \comp -> (
                        if (a == 0x4000 ) 
                         then gcdI1 comp
                         else if (a == 0x4004 )
                           then gcdI2 comp
                           else fromIntegral $ M.loadWord (program comp) (fromIntegral a :: Int) -- merge that
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
gcdProgram :: S.Map Int Word8
gcdProgram = S.fromList [(0,55),(1,69),(2,0),(3,0),(4,183),(5,69),(6,0),(7,0),(8,147),(9,133),(10,69),(11,0),(12,3),(13,37),(14,5),(15,0),(16,131),(17,165),(18,5),(19,0),(20,239),(21,0),(22,64),(23,1),(24,183),(25,69),(26,0),(27,0),(28,147),(29,133),(30,133),(31,0),(32,35),(33,160),(34,165),(35,0),(36,111),(37,240),(38,223),(39,253),(40,99),(41,2),(42,5),(43,2),(44,99),(45,130),(46,5),(47,2),(48,99),(49,250),(50,165),(51,0),(52,147),(53,130),(54,5),(55,0),(56,147),(57,5),(58,5),(59,0),(60,19),(61,133),(62,2),(63,0),(64,111),(65,240),(66,223),(67,254),(68,179),(69,133),(70,165),(71,64),(72,111),(73,240),(74,95),(75,254),(76,19),(77,133),(78,5),(79,0),(80,103),(81,128),(82,0),(83,0)]
-- Merge into 32bits 

oneStep ::  MState ()
oneStep  = do 
  result <- runMaybeT $ do
    pc <- getPC
    setPC (pc + 4)
    i <- loadWord pc
    execute (D.decode D.RV32I $ fromIntegral i)
  case result of
    Nothing -> commit >> (state $ \comp -> ((), comp{exception = True})) -- early return
    Just r -> commit >> return r -- this is a ()

wrap ::  MMIOClash-> MMIOClash
wrap  s = snd $ runState (oneStep ) s

topEntity = 
  mealy (\(iregister,pc) 
                     (gcd1,gcd2) ->
                      let newstate = wrap  MMIOClash{registers = reverse iregister,
                                                      store = Nothing,
                                                      pc=pc,
                                                      gcdI1 = gcd1,
                                                      gcdI2 = gcd2,
                                                      exception = False,
                                                      program = gcdProgram
                                                     }
                      in
                        let storeNext = store newstate
                            npc = nextPC newstate
                        in
                          ((reverse $ registers newstate, npc ), (npc,
                           (\(x,y,z)->x) $ fromMaybe (0,0,(False,False,False,False)) storeNext,
                           (\(x,y,z)->y) $ fromMaybe (0,0,(False,False,False,False)) storeNext,
                           (\(x,y,z)->z) $ fromMaybe (0,0,(False,False,False,False)) storeNext,
                           exception newstate)))
