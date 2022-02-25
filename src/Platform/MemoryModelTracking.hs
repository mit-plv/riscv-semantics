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
import Numeric (showHex)


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
                                               ,relPo
                                               ,let a = helperRel helpAddrDep in
                                                 if length a == 0 then [] else ["addrdep =" <+> (sep.punctuate "+" $ a)]
                                               ,let a = helperRel helpDataDep in
                                                 if length a == 0 then [] else ["datadep =" <+> (sep.punctuate "+" $ a)]
                                               ,let a = helperRel helpCtrlDep in
                                                 if length a == 0 then [] else ["ctrldep =" <+> (sep.punctuate "+" $ a)]
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
        poList = (helpPo 0 . wrapHelper  $poThread 0)
                 ++ (helpPo 1 . wrapHelper $ poThread 1)
        relPo = if length poList == 0 then [] else ["po =" <+> (sep . punctuate "+" $ poList)]
        helpAddrDep = Set.toList $ (S.foldlWithKey (\acc key el ->
                                                   Set.union acc (Set.map (\x -> (x,key)) el))
                                                 Set.empty
                                                 (addr e))
        helpDataDep = Set.toList $ (S.foldlWithKey (\acc key el ->
                                                   Set.union acc (Set.map (\x -> (x,key)) el))
                                                 Set.empty
                                                 (depdata e))
        helpCtrlDep = Set.toList $ (S.foldlWithKey (\acc key el ->
                                                   Set.union acc (Set.map (\x -> (x,key)) el))
                                                 Set.empty
                                                 (ctrl e))
        helperRel = fmap (\(a,b)-> pretty a <> "->" <> pretty b) :: [(Event,Event)]-> [Doc ann]
        wrapHelper (h:t) =
          (h,t)
        wrapHelper [] = (0,[])
        helpPo _th (_last, []) =
          []
        helpPo th (last,(next:t)) =
          ((pretty $ Node(th,last)) <> "->" <> (pretty $ Node (th,next))): helpPo th (next,t)
        declarations =
          sep ["some"
              ,align . vsep $
                [sep ["disj"
                     ,sep $ punctuate comma (memoryEvents ++ (map (\el -> "i"<> el) addressesEvent))
                     ,": MemoryEvent,"]
                ,let a = punctuate comma fenceEvents
                 in
                    if length a /= 0 then sep ["disj", sep a, ": FenceTSO,"] else emptyDoc
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
        threadStarts =
          (case (poThread 0) of
            h:_t -> ["e_0_"<> pretty h<+> "in h1.start"]
            _ -> []) ++
          (case (poThread 1) of
            h:_t -> ["e_1_"<> pretty h<+> "in h2.start"]
            _ -> [])
        _helperFence sMem mem = fmap (\(tid,iid) ->
                                          hcat ["e_", pretty tid, "_", pretty iid]
                                          <+> "in" <+> sMem) $ mem
        helperLdSt sMem mem = fmap (\((tid,iid),addr) ->
                                          hcat ["e_", pretty tid, "_", pretty iid]
                                          <+> "in" <+> sMem <+>"&" <+> "a_" <> pretty addr <>".~address") $ mem

        loadsAndStores = concat
                              [helperLdSt "LoadNormal" loads
                              ,helperLdSt "StoreNormal" stores
                              --,helperFence "FenceTSO" fences
                              ,initAddresses]
        _fences =
          Set.toList . S.foldlWithKey
            (\acc k el ->
              case k of
                Init _ -> acc
                Node(tid,iid) ->
                  case el of
                    EFence-> Set.insert (tid,iid) acc
                    _ -> acc) Set.empty $ lab e
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
          S.foldlWithKey
            (\acc key el ->
                  let aux = case key of
                        Node(_a,_b) -> pretty key : acc
                        Init _ -> acc
                  in
                  case el of
                    ERead _  -> aux
                    EWrite _ _ -> aux
                    EFence -> acc
                  )
             [] $ lab e
        fenceEvents =
          S.foldlWithKey
            (\acc key el ->
                  let aux = case key of
                        Node(_a,_b) -> pretty key : acc
                        Init _ -> acc
                  in
                  case el of
                    ERead _  -> acc
                    EWrite _ _ -> acc
                    EFence -> aux
                  )
             [] $ lab e

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
            dep_ctrl:: Events,
            dep_addr:: Events,
            dep_data:: Events,
            dependencies :: (S.Map Register Events, Events) -- Registers, Ctrl dependencies
} deriving (Show)

-- Translated from Karl Samuel Gruetter's experimentation with our idea to implement dependency tracking with monads
-- based on the remark that the tracking is not dynamic (github.com/mit-plv/riscv-coq)
updateDepsI :: InstructionI -> (S.Map Register Events, Events) -> (S.Map Register Events, Events)
updateDepsI inst d =
  case inst of
     Auipc rd _oimm20 -> (insert rd pc dep,pc)
     Jal rd _jimm20 -> (insert rd pc dep,pc)
     Jalr rd rs1 _oimm12 -> (insert rd pc dep, Set.union pc . fromJust $ S.lookup rs1 dep)
     Beq  rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Bne  rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Blt  rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Bge  rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Bltu rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Bgeu rs1 rs2 _sbimm12 -> branchDep rs1 rs2
     Addi  rd rs1 _ -> immArithDep rs1 rd
     Slti  rd rs1 _ -> immArithDep rs1 rd
     Sltiu rd rs1 _ -> immArithDep rs1 rd
     Xori  rd rs1 _ -> immArithDep rs1 rd
     Ori   rd rs1 _ -> immArithDep rs1 rd
     Andi  rd rs1 _ -> immArithDep rs1 rd
     Slli  rd rs1 _ -> immArithDep rs1 rd
     Srli  rd rs1 _ -> immArithDep rs1 rd
     Srai  rd rs1 _ -> immArithDep rs1 rd
     Add  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Sub  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Sll  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Slt  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Sltu rd rs1 rs2 -> arithDep rs1 rs2 rd
     Xor  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Or   rd rs1 rs2 -> arithDep rs1 rs2 rd
     Srl  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Sra  rd rs1 rs2 -> arithDep rs1 rs2 rd
     And  rd rs1 rs2 -> arithDep rs1 rs2 rd
     Lui rd _imm20 -> memDep
     Lw  rd rs1 _oimm12 -> memDep
     Sw rs1 rs2 _simm12 -> memDep
     Fence _pred _succ -> memDep
     _ -> error "Dependency tracking for an instruction unsupported"
  where
     (dep,pc) = d
     memDep = d
     branchDep rs1 rs2 = (dep, Set.union pc (Set.union (fromJust $ S.lookup rs1 dep) (fromJust $ S.lookup rs2 dep)))
     arithDep rs1 rs2 rd = (insert rd (Set.union (fromJust $ S.lookup rs1 dep) (fromJust $ S.lookup rs2 dep)) dep, pc)
     immArithDep rs1 rd = (insert rd (fromJust $ S.lookup rs1 dep) dep , pc)
     insert (k::Register) v map = if k /= 0 then S.insert k v map else map

type IORead= ReaderT Ptrs IO

data Condition = RegCond (Int64, Register, Int64) | MemCond (Int, Word32)

data Ptrs = Ptrs {
  r_threads :: IORef Minimal64,
  r_currentExecution :: IORef Execution,
  r_currentThread :: IORef Int64,
  r_currentIid :: IORef Int64,
  r_alternativeExplorations :: IORef ([(Event, Event, Execution)]),
  r_revisitSet :: IORef (Set.Set Event),
  r_return :: IORef ([Execution]),
  r_mem :: IORef (MapMemory Int),
  r_postExists :: IORef [Condition]
 -- r_post_forall :: IORef [Condition] -- not needed for simple tests
 -- r_end :: MaybeT IORef () 
}


instance{-# OVERLAPPING #-} RiscvMachine (MaybeT IORead) Int64 where

  getCSRField field = return 0
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
              m <- lift . lift $ readIORef (r_threads refs)
              let ctrlDep = dep_ctrl m
              let dataDep = dep_data m
              let addrDep = dep_addr m
              let newexecution = (execution{
                  domain = Set.union (domain execution) (Set.singleton (Node(tid,iid))),
                  lab = S.insert (Init $ fromIntegral ad) (EWrite (fromIntegral ad) 0) (S.insert (Node(tid,iid)) (ERead $ fromIntegral ad) (lab execution)),
                  rf = S.insert (Node(tid,iid)) w0 (rf execution),
                  addr = S.insert (Node(tid,iid)) addrDep (addr execution),
                  ctrl = S.insert (Node(tid,iid)) ctrlDep (ctrl execution),
                  depdata = S.insert (Node(tid,iid)) dataDep (depdata execution)
                })
              -- add the alternative executions
              --let prefixexecution = prefix (Set.singleton (Node (tid,iid) )) newexecution
              foldM (\_acc el -> do
                        expls <- lift . lift $ readIORef (r_alternativeExplorations refs)
                        --T.trace ("One more load exploration: " ++ (show $ ((Node (tid,iid),el, emptyEx):expls))) return ()
                        lift. lift $ writeIORef (r_alternativeExplorations refs) ((Node (tid,iid),el, emptyEx):expls)) () q
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
                MaybeT (return Nothing) -- b is of type (MaybeT p) a


  storeWord :: forall s. (Integral s, Bits s) => SourceType -> s -> Int32 -> (MaybeT IORead) ()
  storeWord Execute ad val = do
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
                                                 Just (ERead a) -> a == fromIntegral ad
                                                 _ -> False) revisitSet
          --T.trace ("Revisit set on store: " ++ show all_read_same_loc) $ return()
          m <- lift . lift $ readIORef (r_threads refs)
          let ctrlDep = dep_ctrl m
          let dataDep = dep_data m
          let addrDep = dep_addr m
          let newexecution = (execution{
                  domain = Set.union (domain execution) (Set.singleton (Node(tid, iid))),
                  lab = S.insert (Init $ fromIntegral ad) (EWrite (fromIntegral ad) 0) $ S.insert (Node(tid, iid)) (EWrite (fromIntegral ad) (fromIntegral val)) (lab execution),
                  addr = S.insert (Node(tid,iid)) addrDep (addr execution),
                  ctrl = S.insert (Node(tid,iid)) ctrlDep (ctrl execution),
                  depdata = S.insert (Node(tid,iid)) dataDep (depdata execution)
                })
          let prefixexecution = prefix (Set.singleton (Node (tid,iid) )) newexecution

          --T.trace ("Prefix exec: " ++ show prefixexecution) $ return()
          let all_read_not_prefix = all_read_same_loc Set.\\ (domain prefixexecution)
              -- add the alternative executions 
          foldM (\_acc el -> do
                    expls <- lift .lift $ readIORef (r_alternativeExplorations refs)
                    --T.trace ("One more store exploration: " ++ (show $ ((el,Node (tid,iid), prefixexecution):expls))) return ()
                    lift. lift $ writeIORef (r_alternativeExplorations refs) ((el,Node (tid,iid), prefixexecution):expls)) () all_read_not_prefix
          -- update the current execution
          lift .lift $ writeIORef (r_currentExecution refs) newexecution
  fence _start  _end= do --FIXME Todo, for now only supports full fence
      refs <- ask
      tid <- lift. lift $ readIORef (r_currentThread refs)
      iid <- lift . lift $ readIORef (r_currentIid refs)
      lift.lift $ writeIORef (r_currentIid refs) (iid + 1)
      execution <- lift . lift $ readIORef (r_currentExecution refs)
      if ( Set.member (Node (tid, iid)) (domain execution))
        then do
          return ()
        else do
          -- find all the reads which are not in the prefix of a which
          m <- lift . lift $ readIORef (r_threads refs)
          let ctrlDep = dep_ctrl m
          let newexecution = (execution{
                  domain  = Set.union (domain execution) (Set.singleton (Node(tid, iid))),
                  lab     = S.insert (Node(tid, iid)) EFence (lab execution),
                  addr    = (addr execution),
                  ctrl    = S.insert (Node(tid,iid)) ctrlDep (ctrl execution),
                  depdata = (depdata execution)
                })
          lift .lift $ writeIORef (r_currentExecution refs) newexecution



checkGraph :: Execution -> IO Bool
checkGraph g = do
--  putStrLn "New load event, assume an RF, check mem model by calling alloy. Press enter to continue"
--  _wait <- getLine
  let alloyFile = renderStrict . layoutPretty defaultLayoutOptions $ pretty g
--  T.trace "Alloy graph to check:" $ return ()
--  T.putStr alloyFile
  T.writeFile "temporary.als" $! alloyFile
  let callAlloy = proc "java"
        $ ["-cp", "riscv-memory-model/riscvSemantics.jar:riscv-memory-model/alloy4.2_2015-02-22.jar", "RiscvSemantics", "temporary.als" ]
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode callAlloy "stdin"
--  T.trace ("Alloy returned:\n" ++ show (exitCode ) ++ "\n stdOut: " ++ stdout ++ "\n stderr:"++stderr) $ return ()
  let rmTemp= proc "rm"
        $ ["temporary.als"]
  (exitCodeRm, _stdout, _stderr) <- readCreateProcessWithExitCode rmTemp "stdin"
--  T.trace ("rm returned:" ++ show (exitCodeRm == ExitSuccess)) $ return ()
  return (exitCode == ExitSuccess)



prefix1 :: Execution -> Events -> Events
prefix1 ex events =
  let rf_set = Set.foldl (\newset el -> Set.union newset $ case  (S.lookup el (rf ex)) of
                                                              Just l -> Set.singleton l
                                                              Nothing -> Set.empty) Set.empty events in
--  T.trace (show rf_set) $                                                                             
  let ctrl_set = Set.foldl (\newset el -> Set.union newset $ fromMaybe Set.empty (S.lookup el (ctrl ex))) Set.empty events in
  let addr_set = Set.foldl (\newset el -> Set.union newset $ fromMaybe Set.empty (S.lookup el (addr ex))) Set.empty events in
  let data_set = Set.foldl (\newset el -> Set.union newset $ fromMaybe Set.empty (S.lookup el (depdata ex))) Set.empty events in
  Set.union events (Set.union
                                            (Set.union (rf_set) ctrl_set)
                                            (Set.union addr_set data_set))

restrictExec :: Execution -> Events -> Execution
restrictExec ex newdomain =
  ex{domain = Set.intersection (domain ex) newdomain,
     rf = S.restrictKeys (rf ex) newdomain,
     ctrl = S.restrictKeys (ctrl ex) newdomain,
     depdata = S.restrictKeys (depdata ex) newdomain,
     addr = S.restrictKeys (addr ex) newdomain,
     lab = S.restrictKeys (lab ex) newdomain  }

prefix :: Events -> Execution -> Execution
prefix stableSet ex =
  --T.trace ("prefix widening:" ++ show stableSet) $
  let newacc = prefix1 ex stableSet in
    if newacc == stableSet
      then restrictExec ex newacc
      else prefix newacc ex

--prefix :: Events -> Execution -> Execution
--prefix stableSet ex = 
--  let newex = prefix_ stableSet ex in
--    restrictExec newex (domain newex)


unionEx :: Execution -> Execution -> Execution
unionEx u1 u2 =
--  if Set.disjoint (domain u1) (domain u2)
--    then 
      emptyEx{domain = Set.union (domain u1) (domain u2),
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
    --T.trace ("Remove max new:" ++ (show $ length expls)) $ return ()
    case expls of
      [] -> return Nothing
      _ -> do
         let reads = fmap (\((Node(tid,iid)),_,_) -> (tid,iid)) expls
         let maxRead = Set.findMax . Set.fromList $ reads
         let posReturn = fromJust $ L.findIndex (\(e,_,_) -> e == Node maxRead ) expls
         let start = take posReturn expls
         let (retVal:end) = drop posReturn expls
         lift $ writeIORef (r_alternativeExplorations refs) (start++end)
         test <- lift $ readIORef (r_alternativeExplorations refs)
--         T.trace ("Remove max new:" ++ (show $ length test)) $ return ()
         return $ Just retVal



interpThread :: (RiscvMachine p t) => t -> t -> (Instruction -> p ())  -> (p) ()
interpThread pcStart pcStop preExecute = do
    setPC pcStart
    commit
    loop
    where loop = do
              vpc <- getPC
              -- T.trace (showHex (fromIntegral vpc) "") $ return ()
              inst <- loadWord Fetch vpc
              if not (vpc == pcStop)
                then do
                  setPC (vpc + 4)
                  preExecute $ dInst inst
                  execute $ dInst inst
                  commit
                  loop
                else
                  return $! ()
          dInst inst = decode RV64I ((fromIntegral :: Int32 -> MachineInt) inst)
          execute inst =
            case inst of
              IInstruction   i     -> I.execute   i
              _ -> T.trace (show inst) $ error "instruction not supported (not I Instruction)"


instGenAddrCtrlData :: InstructionI -> (MaybeT IORead) ()
instGenAddrCtrlData inst =
  case inst of
    Auipc rd _oimm20      -> noDep
    Jal rd _jimm20        -> noDep
    Jalr rd rs1 _oimm12   -> noDep
    Beq  rs1 rs2 _sbimm12 -> noDep
    Bne  rs1 rs2 _sbimm12 -> noDep
    Blt  rs1 rs2 _sbimm12 -> noDep
    Bge  rs1 rs2 _sbimm12 -> noDep
    Bltu rs1 rs2 _sbimm12 -> noDep
    Bgeu rs1 rs2 _sbimm12 -> noDep
    Addi  rd rs1 _        -> noDep
    Slti  rd rs1 _        -> noDep
    Sltiu rd rs1 _        -> noDep
    Xori  rd rs1 _        -> noDep
    Ori   rd rs1 _        -> noDep
    Andi  rd rs1 _        -> noDep
    Slli  rd rs1 _        -> noDep
    Srli  rd rs1 _        -> noDep
    Srai  rd rs1 _        -> noDep
    Add  rd rs1 rs2       -> noDep
    Sub  rd rs1 rs2       -> noDep
    Sll  rd rs1 rs2       -> noDep
    Slt  rd rs1 rs2       -> noDep
    Sltu rd rs1 rs2       -> noDep
    Xor  rd rs1 rs2       -> noDep
    Or   rd rs1 rs2       -> noDep
    Srl  rd rs1 rs2       -> noDep
    Sra  rd rs1 rs2       -> noDep
    And  rd rs1 rs2       -> noDep
    Lui rd _imm20         -> noDep
    Fence _pred _succ     -> noDep
    Lw  rd rs1 _oimm12    -> do
      refs <- ask
      m <- lift . lift $! readIORef (r_threads refs)
      let (dep,pcDepOld) = dependencies m
      tid <- lift. lift $ readIORef (r_currentThread refs)
      iid <- lift . lift $ readIORef (r_currentIid refs)
      let newdep = insert rd (Set.singleton $ Node(tid,iid)) dep
      let (reg_dep, pc_dep) = dependencies m
      lift . lift . writeIORef (r_threads refs) $!
          m{dep_addr = fromJust $! S.lookup rs1 reg_dep
           ,dep_data = Set.empty
           ,dep_ctrl = pc_dep
           ,dependencies= (newdep,pcDepOld)}
    Sw rs1 rs2 _simm12 -> do -- rs1 is address, rs2 is data
      refs <- ask
      m <- lift . lift $! readIORef (r_threads refs)
      let (reg_dep, pc_dep) = dependencies m
      lift . lift . writeIORef (r_threads refs) $!
          m{dep_addr = fromJust $! S.lookup rs1 reg_dep
           ,dep_data = fromJust $! S.lookup rs2 reg_dep
           ,dep_ctrl = pc_dep}
    _ -> error "instruction not supported in this memory model exploration"
  where
     noDep = return ()
     insert (k::Register) v map = if k /= 0 then S.insert k v map else map




updateDeps :: Instruction -> (MaybeT IORead) ()
updateDeps inst =
            case inst of
              IInstruction   i     -> do
                instGenAddrCtrlData i
                refs <- lift $ ask
                m <- lift . lift $! readIORef (r_threads refs)
                lift . lift .writeIORef (r_threads refs) $ m{dependencies = updateDepsI i (dependencies m)}
              _ -> error "instruction not supported (not I Instruction)"


interpThreads :: Minimal64 -> [(Int64,Int64)] -> [(Int64, Minimal64)] -> (MaybeT IORead) [(Int64, Minimal64)]
interpThreads initMachine ((start,stop):q) acc = do
    T.trace ("\t" ++ showHex (fromIntegral start) "") $ return ()
    refs <- lift $ ask
    -- inc thread, restart iid to 0, run thread
    lift . lift $! writeIORef (r_threads refs) initMachine
    tid <- lift . lift $ readIORef (r_currentThread refs)
    T.trace ("\t\ttid = " ++ show tid) $ return ()
    interpThread start stop updateDeps
    endState <- lift . lift $! readIORef (r_threads refs)
    lift . lift $ writeIORef (r_currentIid refs) (0)
    lift . lift $ writeIORef (r_currentThread refs) (tid+1)
    -- keeps going
    interpThreads initMachine q ((tid, endState) : acc)
interpThreads _initMachine [] acc =
    return acc

readProgram :: String -> IO (Maybe Int64, [(Int, Word8)])
readProgram f = do
    mem <- readElf f
--    T.trace (show $ mem) $ return()
    maybeToHostAddress <- readElfSymbol "tohost" f
    return (fmap (fromIntegral:: Word64 -> Int64) maybeToHostAddress, mem)

getThreads :: String -> Integer -> IO [(Int64, Int64)]
getThreads f n = do
  maybeStart <- readElfSymbol ("_startTh" ++ show n) f
  maybeEnd <- readElfSymbol ("_endTh" ++ show n) f
  case (maybeStart, maybeEnd) of
    (Just startA, Just endA) -> do
      ts <- getThreads f (n+1)
      return $ ((fromIntegral:: Word64 -> Int64) startA, (fromIntegral:: Word64 -> Int64) endA) : ts
    (_, _) -> return []

readLitmusProgram :: String -> IO ([(Int, Word8)], [(Int64, Int64)])
readLitmusProgram f = do
    mem <- readElf f
--    T.trace (show $ mem) $ return()
    threads <- getThreads f 0
    T.trace (show threads) $ return ()
    return (mem, threads)

readLitmusPost :: String -> IO [Condition]
readLitmusPost postfile = do
  T.trace "HARDCODED POSTCONDITION TODO FIX" $ return ()
  return [RegCond (0, 5, 0), RegCond (1, 5, 0)]
  -- return [MemCond (0x100c0, 2), MemCond (0x100c4, 2)]

runFile :: String -> [(Int64, Int64)] -> ReaderT Ptrs IO [Execution]
runFile f threads = do
  (_maybeToHostAddress, program) <- lift $ readProgram f
--  T.trace (show $ program) $ return()
  let mem = S.fromList program
  let c = Minimal64 { registers = S.empty,
                      csrs = (resetCSRFile 64), -- here but probably usless
                      pc = 0x80000000, -- useless will be overwritten
                      nextPC = 0,
                      privMode = Machine,
                      dep_addr = Set.empty,
                      dep_data = Set.empty,
                      dep_ctrl= Set.empty,
                      dependencies = (S.fromList initList, Set.empty)
                      }
  refs <- ask
  lift  $ writeIORef (r_mem refs) (MapMemory { bytes = mem, reservation = Nothing })
  runTime c threads
  where initList = [(i,Set.empty) | i <- [0..31]]:: [(Register,Events)]

runLitmusFile :: String -> String -> ReaderT Ptrs IO [Execution]
runLitmusFile asmfile postfile = do
  (program, threads) <- lift $ readLitmusProgram asmfile
  postcond <- lift $ readLitmusPost postfile
--  T.trace (show $ program) $ return()
  let mem = S.fromList program
  let c = Minimal64 { registers = S.empty,
                      csrs = (resetCSRFile 64), -- here but probably usless
                      pc = 0x80000000, -- useless will be overwritten
                      nextPC = 0,
                      privMode = Machine,
                      dep_addr = Set.empty,
                      dep_data = Set.empty,
                      dep_ctrl= Set.empty,
                      dependencies = (S.fromList initList, Set.empty)
                      }
  refs <- ask
  lift  $ writeIORef (r_mem refs) (MapMemory { bytes = mem, reservation = Nothing })
  lift  $ writeIORef (r_postExists refs) postcond
  runTime c threads
  where initList = [(i,Set.empty) | i <- [0..31]]:: [(Register,Events)]

checkLitmusPosts :: [(Int64, Minimal64)] -> MapMemory Int -> [Condition] -> IO ()
checkLitmusPosts endStates endMem (RegCond (tid, reg, val) : tl) = do
  case L.find (\(t,_) -> t == tid) endStates of
    Just (_, endState) -> do
      case S.lookup reg (registers endState) of
        Just regVal -> 
          if regVal == val then
            checkLitmusPosts endStates endMem tl
          else do
            putStrLn "\n\"exists\" postconditions pass\n"
            return ()
        Nothing -> do
          putStrLn $ "condition supplied for register " ++ show reg ++ " which was not set"
          checkLitmusPosts endStates endMem tl
    Nothing -> do
      putStrLn $ "condition supplied for tid " ++ show tid ++ " which does not exist"
      checkLitmusPosts endStates endMem tl
checkLitmusPosts endStates endMem (MemCond (addr, val) : tl) = do
  let memVal = M.loadWord endMem addr
  if memVal == val then
    checkLitmusPosts endStates endMem tl
  else do
    putStrLn "\n\"exists\" postconditions pass\n"
    return ()  
checkLitmusPosts endStates endMem [] = do
  putStrLn "\n\n\nFAILURE: all \"exists\" postconditions were met, indicating failure\n\n\n"
  return ()

runTime :: Minimal64 -> [(Int64, Int64)] -> ReaderT Ptrs IO [Execution]
runTime init threads = do
  refs <- ask
  lift $ writeIORef (r_currentIid refs) (0)
  lift $ writeIORef (r_currentThread refs) (0)
  result <- runMaybeT (interpThreads init threads [])
  case result of
    Just endStates -> do
      lift $ putStrLn "\n=====\nFound a new execution\n ======="
      refs <- ask
      -- lift $ print $ map (\(a,_) -> a) endStates
      oldexpl <- lift $ readIORef (r_currentExecution refs)
      -- Final mm check because we don't check on stores. Maybe we should check earlier in stores?
      mmOk <- liftIO $ checkGraph oldexpl
      if mmOk then do
        ret <- lift $ readIORef (r_return refs)
        let alloyFile = renderStrict . layoutPretty defaultLayoutOptions $ pretty oldexpl
        lift $ T.putStr alloyFile
        lift $ T.writeFile ("execution"++ (show $ length ret ) ++".als") alloyFile
        lift $ writeIORef (r_return refs) (oldexpl:ret)
        postExists <- lift $ readIORef (r_postExists refs)
        endMem <- lift $ readIORef (r_mem refs)
        lift $ checkLitmusPosts endStates endMem postExists
        nextRound
      else do
        T.trace "failed mmOk" $ return ()
        -- let alloyFile = renderStrict . layoutPretty defaultLayoutOptions $ pretty oldexpl
        -- lift $ T.putStr alloyFile
        -- endMem <- lift $ readIORef (r_mem refs)
        -- lift $ print endMem
        nextRound
    Nothing -> do
      nextRound
  where
    nextRound = do
      refs <- ask
      removed <- removeMax
      case removed of
        Just (r,w,nex) -> do
            t <- lift $ readIORef (r_revisitSet refs)
            oldexpl <- lift $ readIORef (r_currentExecution refs)
            let beforeR = restrict (domain oldexpl) r
            let newexp = unionEx ( restrictExec oldexpl (domain $ prefix beforeR oldexpl)) nex
--            T.trace ("new exec after removeMax:" ++ show (newexp) ++ show (lab newexp)) $ return ()
            let newexpWithRf = newexp {rf = S.insert r w (rf newexp)}
            let newrevisit = restrict t r
            lift $ writeIORef (r_currentIid refs) (0)
            lift $ writeIORef (r_currentThread refs) (0)
            lift $ writeIORef (r_revisitSet refs) newrevisit
            lift $ writeIORef (r_currentExecution refs) newexpWithRf
            runTime init threads
        Nothing -> lift $ readIORef (r_return refs)
    restrict domain elem =
      Set.filter (\el -> case el of
                    Init _ -> True
                    Node(tid',iid') -> case elem of
                      Init _ -> error "restrict to init elem does not make sense"
                      Node(tid,iid) -> tid' < tid || (tid == tid' && iid' <= iid)) domain


mpRev = Platform.MemoryModelTracking.runFile
 "./test/build/mp64"
 $ L.reverse [(0x10078,0x10098),(0x1009c,0x100b4)]
mp = Platform.MemoryModelTracking.runFile
 "./test/build/mp64"
 $ [(0x10078,0x10098),(0x1009c,0x100b0)]

mpFence = Platform.MemoryModelTracking.runFile
 "./test/build/mpFence64"
 $ [(0x10078,0x10098),(0x1009c,0x100b8)]

mpFenceRev = Platform.MemoryModelTracking.runFile
 "./test/build/mpFence64"
 $ L.reverse [(0x10078,0x10098),(0x1009c,0x100b8)]


sbData = Platform.MemoryModelTracking.runFile
 "./test/build/sbData64"
 $ [(0x10078,0x10098),(0x1009c,0x100b4)]

sbDataRev = Platform.MemoryModelTracking.runFile
 "./test/build/sbData64"
 $ L.reverse [(0x10078,0x10098),(0x1009c,0x100b4)]

sbLitmus = Platform.MemoryModelTracking.runLitmusFile
 "./test/litmus/SB.litmus.exe" "./test/litmus/SB.litmus.post"

litmus22W = Platform.MemoryModelTracking.runLitmusFile
 "./test/litmus/2+2W.litmus.exe" "./test/litmus/2+2W.litmus.post"


emptyEx = Execution {
            domain= Set.empty,
            lab= S.empty,
            rf= S.empty,
            addr= S.empty,
            ctrl= S.empty,
            depdata= S.empty}

--readerRead :: IO()
readerRead prog = do
  threads <- newIORef Minimal64{}
  execution <- newIORef emptyEx
  tid <- newIORef 0
  iid <- newIORef 0
  alternatives <- newIORef []
  revisit <- newIORef Set.empty
  returnEx <- newIORef []
  mem <- newIORef $ MapMemory S.empty Nothing
  endStates <- newIORef $ []
  postExs <- newIORef $ []
  runReaderT prog Ptrs{ r_threads= threads,
                      r_currentExecution= execution,
                      r_currentThread=tid,
                      r_currentIid= iid,
                      r_alternativeExplorations= alternatives,
                      r_revisitSet= revisit,
                      r_return= returnEx,
                      r_mem= mem,
                      r_postExists= postExs }
