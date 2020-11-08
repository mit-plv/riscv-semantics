module Platform.Plic
  (Plic,
   toDo,
   ChangeMIP(..),
   plicSetIRQ,
   plicUpdateMIP,
   readPlic,
   writePlic,
   initPlic) where
import Data.Int
import Data.Bits
import Control.Concurrent.MVar

data ChangeMIP =
  Set | Reset | DoNothing deriving(Eq, Show)

data Plic = Plic { toDo :: MVar ChangeMIP,
                   plicServedIrq :: MVar Int32,
                   plicPendingIrq :: MVar Int32}

-- The boolean indicate if we need to set/reset mip in the RV machine.
writePlic :: Plic -> Int32 -> Int32 -> IO ChangeMIP
writePlic plic offset val = do
  let servedIrqs = plicServedIrq plic
  case offset of
    0x200004 -> do -- PLIC_HART_BASE + 4
      let realval = val - 1
      if val < 32
        then do
        vServedIrqs <- takeMVar servedIrqs
        putMVar servedIrqs $ vServedIrqs .&. (complement $ shiftL 1 (fromIntegral realval))
        doUpdate <- plicUpdateMIP plic
        return doUpdate
        else do
        return DoNothing
    _ -> return DoNothing

-- The boolean indicate if we need to set/reset mip
readPlic :: Plic -> Int32 -> IO (Int32,ChangeMIP)
readPlic plic offset = do
  case offset of
    0x200000 -> do -- PLIC_HART_BASE
      return (0, DoNothing)
    0x200004 -> do -- PLIC_HART_BASE + 4
      vPendingIrqs <- readMVar $ plicPendingIrq plic
      vServedIrqs <- takeMVar $ plicServedIrq plic
      let mask = vPendingIrqs .&. (complement vServedIrqs)
      if mask /= 0
        then do
        let i = countTrailingZeros mask --todo ctz32?
        putMVar (plicServedIrq plic) $ vServedIrqs .|. (shiftL 1 i)
        doUpdate <- plicUpdateMIP plic
        return (fromIntegral $ i + 1, doUpdate)
        else do
        putMVar (plicServedIrq plic) $ vServedIrqs
        return (0,DoNothing)
    _ -> return (0,DoNothing)

-- Interal function, used to compute if we need to tell the caller of read/write/set to set the MIP register.
plicUpdateMIP :: Plic -> IO ChangeMIP
plicUpdateMIP plic = do
  pendings <- readMVar $ plicPendingIrq plic
  served <- readMVar $ plicServedIrq plic
  putStrLn $ "Status Plic: " ++ show (pendings) ++ " " ++ show served 
  if (pendings .&. (complement served)) /= 0
    then return Set
    else return Reset

-- Function called by different devices to raise an external
-- interrupt. Return if we need to setup mip.
plicSetIRQ :: Plic -> Int -> Int -> IO ChangeMIP
plicSetIRQ plic irqNum state = do
  let pendingIrqs = plicPendingIrq plic
  let mask = shiftL 1 (irqNum)
  if (state /= 0)
    then do
    vPendingIrqs <- takeMVar pendingIrqs
    putStrLn . show $  vPendingIrqs .|. mask
    putMVar pendingIrqs $ vPendingIrqs .|. mask
    else do
    vPendingIrqs <- takeMVar pendingIrqs
    putMVar pendingIrqs $ vPendingIrqs .&. (complement mask)
  plicUpdateMIP plic

initPlic :: IO Plic
initPlic = do
  pendings <- newMVar 0
  served <- newMVar 0
  td <- newMVar DoNothing
  return $ Plic {toDo = td, plicServedIrq = served, plicPendingIrq = pendings}

