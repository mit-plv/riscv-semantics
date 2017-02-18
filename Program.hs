{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Program where
import Decode
import Data.Bits

class (Monad p, Integral t, Bits t, Integral u, Bits u) => RiscvProgram p t u | p -> t, t -> u where
   getRegister :: Register -> p t
   setRegister :: (Integral s) => Register -> s -> p ()
   load :: (Integral s) => s -> p t
   store :: (Integral r, Integral s) => r -> s -> p ()
   getPC :: p t
   setPC :: (Integral s) => s -> p ()
