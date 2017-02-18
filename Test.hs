import Computer32
import Decode
import Execute
import Control.Monad.State

c = Computer32 { registers = [0,0,0,0], pc = 5, mem = [0,0,0,0] }
action = do
  execute (Lui 1 19)
  execute (Lui 2 23)
  execute (Lui 4 1)
  execute (Add 3 1 2)
  execute (Sw 4 3 0)
cp = runState action c
