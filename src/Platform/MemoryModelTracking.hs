{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, InstanceSigs, AllowAmbiguousTypes, OverloadedStrings #-}
module Platform.MemoryModelTracking where
import Spec.Machine
import Spec.Decode
import Utility.Utility
import Spec.CSRFile
import qualified Spec.CSRField as Field
import qualified Spec.Memory as M
import Utility.MapMemory
import Data.Bits
import Data.Int
import qualified Data.List as L 
import Data.Word
import Data.Maybe
import qualified Data.Map.Strict as S
import Control.Monad.State
--import Data.SBV
import qualified Data.Set as Set
import Control.Monad.Reader
import Data.IORef
import Data.Array.IO
import Control.Monad.Trans.Maybe

import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import System.Exit
import System.Process 
 
import Spec.ExecuteI as I
import Utility.Elf
import Debug.Trace as T


data Event =  Init Int64 --location begin initialized
  | Node (Int64, Int64) deriving (Eq,Show, Ord)

instance Pretty Event where
  pretty (Node(t,i)) = hcat ["e_", pretty t , "_", pretty i]
  pretty (Init(i)) = "ia_" <> pretty i

type Events = Set.Set Event -- Set of events

data Lab = ERead Int64
  | EWrite Int64 Int32
  | EFence
  | EError deriving (Eq,Show, Ord)

data Execution = Execution {
            domain :: Events,
            lab :: S.Map Event Lab,
            rf :: S.Map Event Event,
            addr :: S.Map Event Events,
            ctrl :: S.Map Event Events,
            depdata :: S.Map Event Events}

instance Pretty Execution where
  pretty e =
    vsep 
      ["open riscv"
      ,"run MP"
        <+> braces formula
        <+> "for"
        <+> pretty depth]
      where 
        depth = (8::Integer)
        formula = indent 2 $ vsep
          [declarations
          ,"all h : Hart, m, im : MemoryEvent |"
          ,indent 2 . vsep . punctuate " and" $ concat [forallConstraints
                                               ,threadStarts
                                               ,loadsAndStores
                                               ,[relPo] 
                                               ,[relRf] 
                                               ,["RISCV_mm"]]]
        poThread ith =
          L.sort . catMaybes . fmap
            (\el -> case el of
                      Node(t, i) -> if t == ith then Just i else Nothing
                      Init _ -> Nothing ) 
            . Set.toList $ domain e
        pair2Doc ((tid,iid),(-1,addr)) = pretty (Init addr) <> "->" <> pretty (Node(tid,iid))
        pair2Doc ((tid,iid),(tidw,iidw)) = pretty (Node (tidw,iidw)) <> "->" <> pretty (Node(tid,iid))
        rfHelper = 
          Set.toList . S.foldlWithKey 
            (\acc rd wr -> 
              case rd of
                Init _ -> error "a read is living in the domain of the rf map, absurd"
                Node(tid,iid) -> 
                  case wr of
                    Init addr -> Set.insert ((tid,iid),(-1,addr)) acc
                    Node (tidw, iidw) -> Set.insert ((tid,iid),(tidw,iidw)) acc) Set.empty $ rf e
        relRf = "rf =" <+> (sep . punctuate "+" . map pair2Doc $ rfHelper) 
        relPo = "po =" <+> (sep . punctuate "+" $ (helpPo 0 . wrapHelper $ T.trace (show$ poThread 0) $poThread 0)
                                                  ++ (helpPo 1 . wrapHelper $ poThread 1))
        wrapHelper (h:t) =
          (h,t) 
        wrapHelper [] = (0,[])
        helpPo _th (_last,[]) =
          []
        helpPo th (last,(next:t)) =
          ((pretty $ Node(th,last)) <> "->" <> (pretty $ Node (th,next))): helpPo th (next,t) 
        declarations = 
          sep ["some"
              ,align . vsep $ 
                [sep ["disj"
                     ,sep $ punctuate comma (memoryEvents ++ (map (\el -> "i"<> el) addressesEvent))
                     ,": MemoryEvent,"]
                ,sep ["disj"
                     ,sep $ punctuate comma addressesEvent
                     ,": Address,"]
                ,sep ["disj"
                     ,"h1, h2 : Hart |"]]]
        forallConstraints = [parens "h = h1 or h = h2" 
                            ,parens ("im in Init => " <+>
                                    parens (sep $ punctuate " or" $ map (\el -> "im = " <+> "i" <> el) addressesEvent))
                            ,parens ("m in NonInit => " <+>
                                    parens (sep $ punctuate " or" $ map (\el -> "m = " <+> el) memoryEvents))]
        threadStarts = "e_0_0 in h1.start":if poThread 1 == [] then [] else ["e_1_0 in h2.start"] --TODO FIXME hardcoded for 2 threads 
        helperLdSt sMem mem = fmap (\((tid,iid),addr) ->
                                          hcat ["e_", pretty tid, "_", pretty iid]
                                          <+> "in" <+> sMem <+>"&" <+> "a_" <> pretty addr <>".~address") $ mem
        loadsAndStores = concat 
                              [helperLdSt "LoadNormal" loads
                              ,helperLdSt "StoreNormal" stores
                              ,initAddresses]
        loads = 
          Set.toList . S.foldlWithKey 
            (\acc k el -> 
              case k of
                Init _ -> acc
                Node(tid,iid) -> 
                  case el of
                    ERead addr -> Set.insert ((tid,iid),addr) acc
                    _ -> acc) Set.empty $ lab e
        stores = 
          Set.toList . S.foldlWithKey 
            (\acc k el -> 
              case k of
                Init _ -> acc
                Node(tid,iid) -> 
                  case el of
                    EWrite addr _ -> Set.insert ((tid,iid),addr) acc
                    _ -> acc) Set.empty $ lab e
        memoryEvents =
          fmap
            (\el -> case el of
                      Node(t,i) -> "e_" <> pretty t <> "_" <> pretty i
                      Init r -> "i_" <> pretty r) 
            . Set.toList $ domain e
        addressesEvent = 
          fmap (\el -> "a_" <> pretty el) . Set.toList . S.foldl 
            (\acc el -> case el of
                          EWrite addr _ -> Set.insert addr acc
                          ERead addr  -> Set.insert addr acc
                          _ -> acc) Set.empty $ lab e
        initAddresses = 
          fmap (\el -> "ia_" <> pretty el <+> "in Init &"<> "a_"<>pretty el <>".~address") . Set.toList . S.foldl 
            (\acc el -> case el of
                          EWrite addr _ -> Set.insert addr acc
                          ERead addr  -> Set.insert addr acc
                          _ -> acc) Set.empty $ lab e

 

instance Show(Execution) where
  show a = show (domain a)

data Minimal64 = Minimal64 { 
            registers :: S.Map Register Int64,
            csrs :: CSRFile,
            pc :: Int64,
            nextPC :: Int64,
            privMode :: PrivMode,
            dependencies :: S.Map Register Events,
            ctrlDependency :: Events
} deriving (Show) 

type IORead= ReaderT Ptrs IO

data Ptrs = Ptrs {
  r_threads :: IORef Minimal64,
  r_currentExecution :: IORef Execution,
  r_currentThread :: IORef Int64,
  r_currentIid :: IORef Int64,
  r_alternativeExplorations :: IORef ([(Event, Event, Execution)]),
  r_revisitSet :: IORef (Set.Set Event),
  r_return :: IORef ([Execution]),
  r_mem :: IORef (MapMemory Int)--,
 -- r_end :: MaybeT IORef () 
}


instance{-# OVERLAPPING #-} RiscvMachine (MaybeT IORead) Int64 where
  getPC = do
      refs <- ask
      --tid <- lift $ readIORef (r_currentThread refs)
      machine <- lift . lift $! readIORef (r_threads refs)
      return (pc machine)
  setPC npc = do
      refs <- ask 
      machine <- lift . lift $! readIORef (r_threads refs)
      lift . lift $! writeIORef (r_threads refs) (machine{nextPC = npc})
  getPrivMode = do
      refs <- ask
      machine <- lift . lift $! readIORef (r_threads refs)
      return (privMode machine) 
  setPrivMode val = do
      refs <- ask
      machine <- lift . lift $! readIORef (r_threads refs)
      lift . lift $! writeIORef (r_threads refs) (machine{privMode = val})
  commit = do
      refs <- ask
      machine <- lift . lift $! readIORef (r_threads refs)
      lift . lift $! writeIORef (r_threads refs) (machine{pc = nextPC machine})
  getRegister reg = do
      refs <- ask
      machine <- lift . lift $! readIORef (r_threads refs)
      return (if reg == 0 then 0 else fromMaybe 0 (S.lookup reg (registers machine)))
  setRegister reg val = do
      refs <- ask
      machine <- lift . lift $! readIORef (r_threads refs)
      let newmachine = if reg == 0 then machine else machine { registers = S.insert reg (fromIntegral val) (registers machine) }
      lift . lift $! writeIORef (r_threads refs) newmachine 
  loadWord :: forall s. (Integral s) => SourceType -> s -> (MaybeT IORead) Int32
  loadWord Fetch ad = do
       refs <-  ask
       b0 <- lift . lift $ readIORef (r_mem refs) 
       return $  (fromIntegral:: Word32 -> Int32) $ M.loadWord b0 ((fromIntegral :: s -> Int) ad)
  loadWord Execute ad = do
      refs <- ask
      tid <- lift . lift $ readIORef (r_currentThread refs)
      iid <- lift . lift $ readIORef (r_currentIid refs)
      lift . lift $ writeIORef (r_currentIid refs) (iid + 1)
      execution <- lift . lift $ readIORef (r_currentExecution refs)
      if ( Set.member (Node (tid, iid)) (domain execution))
        then do
            let rf' = fromJust $ S.lookup  (Node (tid, iid)) (rf execution)
            case rf' of
              Init _ -> return 0
              Node (tid', iid') -> do
                let lab' = fromJust $ S.lookup  (Node (tid', iid')) (lab execution)
                case lab' of
                  EWrite _ d -> return d
                  _ -> error "Not a write in rf"
        else do
          -- Update the revisit set to add this reads
          t <- lift . lift $ readIORef (r_revisitSet refs)
          lift . lift $ writeIORef (r_revisitSet refs)
            (Set.union (Set.singleton (Node(tid,iid))) t)
          let all_write_same_loc = fmap fst . S.toList $ S.filter (\l -> case l of
                        EWrite a _b -> a == fromIntegral ad 
                        _ -> False) ( S.insert (Init $ fromIntegral ad) (EWrite (fromIntegral ad) 0) $ lab execution)
          case all_write_same_loc of
            [] -> error "reading uninitialized location"
            w0:q -> do -- q are the alternative writes to push in the alternativeExploration structure 
              let newexecution = (execution{
                  domain = Set.union (domain execution) (Set.singleton (Node(tid,iid))),
                  lab = S.insert (Init $ fromIntegral ad) (EWrite (fromIntegral ad) 0) (S.insert (Node(tid,iid)) (ERead $ fromIntegral ad) (lab execution)),
                  rf = S.insert (Node(tid,iid)) w0 (rf execution)
                -- update addr ctrl data dependencies
                })
              -- add the alternative executions
              foldM (\_acc el -> do 
                        expls <- lift . lift $ readIORef (r_alternativeExplorations refs)
                        T.trace ("One more exploration: " ++ (show $ ((Node (tid,iid),el, newexecution):expls))) return ()
                        lift. lift $ writeIORef (r_alternativeExplorations refs) ((Node (tid,iid),el, newexecution):expls)) () q 
              -- update the current execution
              lift. lift $ writeIORef (r_currentExecution refs) newexecution 
              -- finally check the memory model 
              mmOk <- liftIO $ checkGraph newexecution 
              if mmOk
              then do 
                    let lab' = fromJust $ S.lookup  w0 (lab newexecution)
                    case lab' of
                      EWrite _ d -> return d
                      _ -> error "Not a write in rf"
              else do
                T.trace (show "Failed Load") $ MaybeT (return Nothing) -- b is of type (MaybeT p) a
                

  storeWord :: forall s. (Integral s, Bits s) => SourceType -> s -> Int32 -> (MaybeT IORead) ()
  storeWord Execute addr val = do
      refs <- ask
      tid <- lift. lift $ readIORef (r_currentThread refs)
      iid <- lift . lift $ readIORef (r_currentIid refs)
      lift.lift $ writeIORef (r_currentIid refs) (iid + 1)
      execution <- lift . lift $ readIORef (r_currentExecution refs)
      revisitSet <- lift . lift $ readIORef (r_revisitSet refs)
      if ( Set.member (Node (tid, iid)) (domain execution))
        then do
          return ()
        else do
          -- find all the reads which are not in the prefix of a which
          let all_read_same_loc =  Set.filter 
                                (\l -> case S.lookup l (lab execution) of
                                                 Just (ERead a) -> a == fromIntegral addr 
                                                 _ -> False) revisitSet
          let newexecution = (execution{
                  domain = Set.union (domain execution) (Set.singleton (Node(tid, iid))),
                  lab = S.insert (Init $ fromIntegral addr) (EWrite (fromIntegral addr) 0) $ S.insert (Node(tid, iid)) (EWrite (fromIntegral addr) (fromIntegral val)) (lab execution)
                -- update addr ctrl data dependencies
                })
          let prefixexecution = prefix (Set.singleton (Node (tid,iid) )) newexecution
          let all_read_not_prefix = all_read_same_loc Set.\\ (domain prefixexecution)
              -- add the alternative executions 
          foldM (\_acc el -> do 
                    expls <- lift .lift $ readIORef (r_alternativeExplorations refs)
                    lift. lift $ writeIORef (r_alternativeExplorations refs) ((el,Node (tid,iid), prefixexecution):expls)) () all_read_not_prefix 
          -- update the current execution
          lift .lift $ writeIORef (r_currentExecution refs) newexecution 


checkGraph :: Execution -> IO Bool 
checkGraph g = do
  putStrLn "New load event, assume an RF, check mem model by calling alloy. Press enter to continue"
  _wait <- getLine
  let alloyFile = renderStrict . layoutPretty defaultLayoutOptions $ pretty g 
  T.trace "Alloy grpah to check:" $ return ()
  T.putStr alloyFile
  T.writeFile "temporary.als" $! alloyFile
  let callAlloy = proc "java"
        $ ["-cp", "riscv-memory-model/riscvSemantics.jar:riscv-memory-model/alloy4.2_2015-02-22.jar", "RiscvSemantics", "temporary.als" ]
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode callAlloy "stdin"
  T.trace ("Alloy returned:\n" ++ show (exitCode ) ++ "\n stdOut: " ++ stdout ++ "\n stderr:"++stderr) $ return ()
  let rmTemp= proc "rm"
        $ ["temporary.als"]
  (exitCodeRm, _stdout, _stderr) <- readCreateProcessWithExitCode rmTemp "stdin"
  T.trace ("rm returned:" ++ show (exitCodeRm == ExitSuccess)) $ return ()
  return (exitCode == ExitSuccess)


  
prefix1 :: Execution -> Events -> Execution
prefix1 ex events =
  let rf_set = S.foldl (\newset el -> Set.union newset (Set.singleton el)) Set.empty (rf ex) in
  let ctrl_set = S.foldl (\newset el -> Set.union newset el) Set.empty (ctrl ex) in
  let addr_set = S.foldl (\newset el -> Set.union newset el) Set.empty (addr ex) in
  let data_set = S.foldl (\newset el -> Set.union newset el) Set.empty (depdata ex) in
  restrictExec ex $ Set.union events (Set.union (Set.union (rf_set) ctrl_set) (Set.union addr_set data_set))

restrictExec :: Execution -> Events -> Execution
restrictExec ex newdomain =
  ex{domain = Set.intersection (domain ex) newdomain,
     rf = S.restrictKeys (rf ex) newdomain,
     ctrl = S.restrictKeys (ctrl ex) newdomain,
     depdata = S.restrictKeys (depdata ex) newdomain,
     addr = S.restrictKeys (addr ex) newdomain,
     lab = S.restrictKeys (lab ex) newdomain  } 

prefix_ :: Events -> Execution -> Execution
prefix_ stableSet ex = 
  let newacc = prefix1 ex stableSet in
    if (domain newacc) == stableSet 
      then newacc 
      else prefix_ (domain newacc) ex

prefix :: Events -> Execution -> Execution
prefix stableSet ex = 
  let newex = prefix_ stableSet ex in
    restrictExec newex (domain newex)


unionEx :: Execution -> Execution -> Execution
unionEx u1 u2 = 
--  if Set.disjoint (domain u1) (domain u2)
--    then 
      Execution{domain = Set.union (domain u1) (domain u2),
       rf = S.union (rf u1) (rf u2),
       lab = S.union (lab u1) (lab u2),
       addr = S.union (addr u1) (addr u2),
       ctrl = S.union (ctrl u1) (ctrl u2),
       depdata = S.union (depdata u1) (depdata u2) }
--    else error "union on nondisjoint keys, sort of expected at this point"

removeMax :: IORead (Maybe (Event, Event, Execution))
removeMax = do
    refs <- ask
    expls <- lift $ readIORef (r_alternativeExplorations refs)
    case expls of
      [] -> return Nothing
      _ -> do
         let reads = fmap (\((Node(tid,iid)),_,_) -> (tid,iid)) expls
         let maxRead = Set.findMax . Set.fromList $ reads
         let posReturn = fromJust $ L.findIndex (\(e,_,_) -> e == Node maxRead ) expls
         let start = take posReturn expls
         let (retVal:end) = drop posReturn expls
         lift $ writeIORef (r_alternativeExplorations refs) (start++end)
         return $ Just retVal



interpThread :: (RiscvMachine p t) => t -> t  -> (p) () 
interpThread pcStart pcStop = do
    setPC pcStart
    commit
    loop
    where loop = do
              vpc <- getPC
              T.trace (show $ fromIntegral vpc) $return () 
              inst <- loadWord Fetch vpc
              if not (vpc == pcStop)
                then do
                  setPC (vpc + 4)
                  execute (decode RV64I ((fromIntegral :: Int32 -> MachineInt) inst))
                  commit
                  loop
                else
                return $! ()
          execute inst =
            case inst of
              IInstruction   i     -> I.execute   i
              _ -> T.trace (show inst) $ error "invalid riscv instruction encountered"

  
interpThreads :: Minimal64 -> [(Int64,Int64)] -> (MaybeT IORead) ()
interpThreads initMachine ((start,stop):q) = do
    refs <- lift $ ask
    -- inc thread, restart iid to 0, run thread
    lift . lift $! writeIORef (r_threads refs) initMachine 
    tid <- lift . lift $ readIORef (r_currentThread refs)
    interpThread start stop
    lift . lift $ writeIORef (r_currentIid refs) (0)
    lift . lift $ writeIORef (r_currentThread refs) (tid+1)
 
    -- keeps going
    interpThreads initMachine q
interpThreads _initMachine [] = 
    return ()

readProgram :: String -> IO (Maybe Int64, [(Int, Word8)])
readProgram f = do
    mem <- readElf f
    T.trace (show $ mem) $ return()
    maybeToHostAddress <- readElfSymbol "tohost" f
    return (fmap (fromIntegral:: Word64 -> Int64) maybeToHostAddress, mem)


runFile :: String -> [(Int64, Int64)] -> ReaderT Ptrs IO [Execution]
runFile f threads = do
  (_maybeToHostAddress, program) <- lift $ readProgram f
  T.trace (show $ program) $ return()
  let mem = S.fromList program
  let c = Minimal64 { registers = S.empty,
                      csrs = (resetCSRFile 64), -- here but probably usless
                      pc = 0x80000000, -- useless will be overwritten
                      nextPC = 0,
                      privMode = Machine,
                      dependencies = S.empty,
                      ctrlDependency = Set.empty
                      }
  refs <- ask
  lift  $ writeIORef (r_mem refs) (MapMemory { bytes = mem, reservation = Nothing }) 
  runTime c threads


runTime :: Minimal64 -> [(Int64, Int64)] -> ReaderT Ptrs IO [Execution]
runTime init threads = do
  refs <- ask
  lift $ writeIORef (r_currentIid refs) (0)
  lift $ writeIORef (r_currentThread refs) (0)
  result <- runMaybeT (interpThreads init threads)
  case result of 
    Just _ -> do
      lift $ putStrLn "Found a new execution"
      refs <- ask
      oldexpl <- lift $ readIORef (r_currentExecution refs)
      ret <- lift $ readIORef (r_return refs)
      let alloyFile = renderStrict . layoutPretty defaultLayoutOptions $ pretty oldexpl 
      lift $ T.writeFile ("execution"++ (show $ length ret ) ++".als") alloyFile 
      lift $ writeIORef (r_return refs) (oldexpl:ret) 
      nextRound
    Nothing -> do 
      lift $ putStrLn "Dead execution path, restart interpreter"
      nextRound
  where
    nextRound = do
      refs <- ask
      removed <- removeMax
      case removed of
        Just (r,w,nex) -> do
            t <- lift $ readIORef (r_revisitSet refs)
            oldexpl <- lift $ readIORef (r_currentExecution refs)
            T.trace (show $ prefix (Set.singleton r) oldexpl) $ return ()
            T.trace (show nex) $ return ()
            let newexp = unionEx (prefix (Set.singleton r) oldexpl) nex
            let newexpWithRf = newexp {rf = S.insert r w (rf newexp)} 
            let newrevisit = Set.intersection t (domain $ prefix (Set.singleton r) oldexpl)
            lift $ writeIORef (r_currentIid refs) (0)
            lift $ writeIORef (r_currentThread refs) (0)
            lift $ writeIORef (r_revisitSet refs) newrevisit 
            lift $ writeIORef (r_currentExecution refs) newexpWithRf 
            runTime init threads
        Nothing -> lift $ readIORef (r_return refs)

ok = Platform.MemoryModelTracking.runFile
 "/home/bthom/git/riscv-semantics/test/build/mp64" 
 [(0x10078,0x10098),(0x1009c,0x100b4)]

--readerRead :: IO()
readerRead = do
  threads <- newIORef Minimal64{}
  execution <- newIORef Execution {
            domain= Set.empty, 
            lab= S.empty,
            rf= S.empty,
            addr= S.empty,
            ctrl= S.empty,
            depdata= S.empty}
  tid <- newIORef 0 
  iid <- newIORef 0 
  alternatives <- newIORef []
  revisit <- newIORef Set.empty
  returnEx <- newIORef []
  mem <- newIORef $ MapMemory S.empty Nothing
  runReaderT ok Ptrs{ r_threads= threads,
                      r_currentExecution= execution, 
                      r_currentThread=tid, 
                      r_currentIid= iid,
                      r_alternativeExplorations= alternatives,
                      r_revisitSet= revisit, 
                      r_return= returnEx, 
                      r_mem= mem } 
