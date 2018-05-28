{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, MultiWayIf #-}

module Etanol.Analysis (
                getField, putField, invokeVirtual, invokeStatic, invokeSpecial,
                isFinalStaticField, dependencies, AnyID(..), uniques,
                basicop, definedAt, nonBasicop
        ) where

-- update what is being imported

import Data.Binary (Word8, Word16, Word32)
import Data.Maybe 
import Data.Map.Strict ((!?), (!))
import qualified Data.Map.Strict as M
import Data.List
import Debug.Trace (trace)
import Control.Monad
import Control.Applicative
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import ByteCodeParser.Instructions
import Etanol.Types
import Etanol.ControlFlowGraph
import Etanol.Decompile
import Etanol.MonadFX

type NamePrefix = String -- prefixes of strings, anynames
type AnyName    = String

initialStrongImpureList :: [NamePrefix] 
initialStrongImpureList = ["java.io", "java.net", "javax.swing", 
                           "java.awt", "java.nio", "java.sql", 
                           "javax.imageio", "javax.print", "javax.sql",
                           "javax.rmi", "javax.sound"]

isInitialStrongImpure :: AnyName -> Bool
isInitialStrongImpure namae = namae `elem` initialStrongImpureList

getField, putField, invokeStatic, invokeSpecial, invokeVirtual :: Word8
getField = 180
putField = 181
invokeVirtual = 182
invokeSpecial = 183
invokeStatic  = 184

-- When reading an index from the bytecode into the constantPool, decrement it by 1, as is done here
-- using pred
toIndex :: [Word8] -> Word32
toIndex xs = pred $ sum $ map (uncurry (*)) $ zip (reverse $ map fromIntegral xs) $ map (2^) [0..] 

isFinalStaticField :: [ConstantInfo] -> FieldDB -> Int -> Maybe Bool
isFinalStaticField cpool fieldDB idx = let      fid     = getFieldName cpool idx
                                                ftype   = fieldDB !? fid
                                       in if isNothing ftype then Nothing 
                                          else if ftype == Just Normal then Just False else Just True 

uniques :: [AnyID] -> [AnyID]
uniques =  map head . group . sort


data AnyID =    EFieldID { fieldID :: FieldID } |
                EMethodID { methodID :: MethodID }
                deriving (Show, Eq, Ord)

data AnyData =  EFieldData { fieldData :: NamedField } |
                EMethodData { methodData :: NamedMethodCode }
                deriving (Show, Eq)

isField :: AnyID -> Bool
isField (EFieldID _)    = True
isField _               = False

isMethod :: AnyID -> Bool
isMethod (EMethodID _)  = True
isMethod _              = False

-- Figure out the direct dependencies of a piece of code given the constant pool
dependencies :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
dependencies cpool codes = uniques $ depsMethod cpool codes   

-- Note that this does not need to account for new or anewarray or multianewarray as
-- their constructors are called shortly after these declarations, including them in
-- the dependencies anyway

depsMethod :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
depsMethod cpool [] = []
depsMethod cpool ((idx, op : rest) : restcode)
        | op `elem` [getField, putField, getStatic, putStatic]          = let   idx = toIndex rest
                                                                                fld = getFieldName cpool $ fromIntegral idx
                                                                          in [EFieldID fld] ++ depsMethod cpool restcode
        | op `elem` [invokeSpecial, invokeStatic, invokeVirtual]        = let   idx = toIndex rest
                                                                                mthd = getMethodName cpool $ fromIntegral idx
                                                                          in [EMethodID mthd] ++ depsMethod cpool restcode
        | otherwise                                                     = depsMethod cpool restcode


data Status = Analyzing | NotAnalyzed
                deriving (Show, Eq, Ord)

type LoadedThings = M.Map AnyID AnyData
type LoadedThingsStatus = M.Map AnyID Status

nonEmpty :: [a] -> Bool
nonEmpty = not . null

-- functions for analysis

-- | analyseAll is the main entry point for analysis
analyseAll ::           [ConstantInfo]  -> 
                        LoadedThings    -> 
                        LoadedThingsStatus ->
                        FieldDB        -> MethodDB              ->                    -- old  
                        (LoadedThingsStatus, FieldDB, MethodDB)                       -- new values
analyseAll cpool loadedThings loadedThingsStatus fDB mDB =
        if M.null loadedThingsStatus then (loadedThingsStatus, fDB, mDB)
        else    let (thing, _) = M.elemAt 0 loadedThingsStatus
                    (loadedThingsStatus', fDB', mDB') = analysisDriver cpool loadedThings loadedThingsStatus fDB mDB thing
                in  analyseAll cpool loadedThings loadedThingsStatus' fDB' mDB'     

-- assumes thing to be in loadedThings
analysisDriver ::       [ConstantInfo]  -> 
                        LoadedThings    -> 
                        LoadedThingsStatus ->
                        FieldDB        -> MethodDB              ->                    -- olds
                        AnyID                                   ->                    -- target  
                        (LoadedThingsStatus, FieldDB, MethodDB)                       -- new values
analysisDriver cpool loadedThings loadedThingsStatus fDB mDB thing = -- thing is guaranteed to be in loadedThings 
        let thingdata = loadedThings ! thing
        in      if isField thing 
                then (M.delete thing loadedThingsStatus, analyseField cpool loadedThings fDB thing, mDB)
                        -- delete object                get the changed field                   old method db works
                        --                              note that we do not pass the current
                        --                              statuses to field analysis as fields are always
                        --                              analyzable in this setting
                else    if (loadedThingsStatus ! thing) == Analyzing -- found loop
                        then    let mID = methodID thing
                                    mDB' = M.insert mID UnanalyzableMethod mDB
                                in  (M.delete thing loadedThingsStatus, fDB, mDB')
                        else    let mID = methodID thing
                                    loadedThingsStatus' = M.insert thing Analyzing loadedThingsStatus
                                    (loadedThingsStatus'', fDB', mDB') = analyseMethod cpool loadedThings loadedThingsStatus' fDB mDB thing
                                in  (M.delete thing loadedThingsStatus'', fDB', mDB')


isBasic :: FieldDescriptor -> Bool
isBasic s = (head s) `elem` ['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z'] 

-- checks if it is final static or basic type atm, assumes AnyID is a isField
analyseField :: [ConstantInfo] -> LoadedThings -> FieldDB -> AnyID -> FieldDB 
analyseField cpool loadedThings fDB thing = 
        let fID = fieldID thing
            ((_, fDesc), fAccessFlags) = fieldData (loadedThings ! thing)
            verdict =   if (AFFinal `elem` fAccessFlags) && (AFStatic `elem` fAccessFlags)
                        then FinalStatic
                        else    if isBasic fDesc
                                then Basic
                                else Normal
        in  M.insert fID verdict fDB

-- | Feeding mechanism through the analysisDriver. See `analyseMethod`
feedAll ::      [ConstantInfo] -> LoadedThings -> LoadedThingsStatus -> FieldDB -> MethodDB -> [AnyID] ->
                (LoadedThingsStatus, FieldDB, MethodDB)
feedAll _     _            loadedThingsStatus fDB mDB []           = (loadedThingsStatus, fDB, mDB)
feedAll cpool loadedThings loadedThingsStatus fDB mDB (aID : rest) =
        let (lth', fdb', mdb') =        if M.member aID loadedThingsStatus 
                                        then analysisDriver cpool loadedThings loadedThingsStatus fDB mDB aID
                                        else (loadedThingsStatus, fDB, mDB)
        in  feedAll cpool loadedThings lth' fdb' mdb' rest

-- checks properties for method
-- currently no recursive method is analyzed to be pure
analyseMethod ::        [ConstantInfo] ->
                        LoadedThings -> LoadedThingsStatus ->
                        FieldDB -> MethodDB ->
                        AnyID ->
                        (LoadedThingsStatus, FieldDB, MethodDB)
analyseMethod cpool loadedThings loadedThingsStatus fDB mDB thing =
        let     mID = methodID thing
                mData = methodData (loadedThings ! thing)
                (_, mCode, _) = mData
                deps = dependencies cpool mCode
                mDeps = filter isMethod deps    -- :: [AnyID]
                fDeps = filter isField deps     -- :: [AnyID]
                mNAl = map EMethodID $ filter (\x -> M.notMember x mDB) $ map methodID mDeps  -- ::[AnyID]
                fNAl = map EFieldID  $ filter (\x -> M.notMember x fDB) $ map fieldID  fDeps  -- ::[AnyID]
                nAl  = mNAl ++ fNAl
                nA  = filter (\x -> M.notMember x loadedThings) nAl 
                        -- not previously analyzed and also not loaded, but required for analysis
        in      if nonEmpty nA
                then (loadedThingsStatus, fDB, M.insert mID UnanalyzableMethod mDB)
                else let        (loadedThingsStatus', fDB', mDB') = feedAll cpool loadedThings loadedThingsStatus fDB mDB nAl
                                -- after this all deps of this method is not in loadedThingsStatus, as they have all been analyzed
                                -- recursively. So we can now safely analyse this method itself
                                -- But it may happen that this method itself got into a loop and was analyzed already
                     in         if M.member thing loadedThingsStatus' 
                                then let verdict  = verdictifyMethod cpool fDB mDB mData
                                     in  (loadedThingsStatus', fDB', M.insert mID verdict mDB')
                                else (loadedThingsStatus', fDB', mDB')

-- analyze method for type when all its dependencies are met
----------------------------------------------------------------------
-- Purity analysis :
-- There are currently 3 types 
-- pure, local, impure, strongimpure
-- a method is strong impure if it transitively calls anything from any of the initialStrongImpureList
-- => a method is strong impure iff it accesses any strong impure thing
-- a method is pure if it only calls no strong impure method, no impure method on parameters, doesn't access non final static fields.
-- and performs mutations on newly created objects only in basic fields.
-- a method is local if it is pure weakened with the condition that it modifies only "this" in basic fields.
-- If a method accesses an unanalyzable thing, it becomes unanalyzable.
-- note that unanalyzable is stronger than strong impure, that is if a method is judged to be both
-- strong impure and unanalyzable then it is unanalyzable, although operationally both of these are
-- the same currently.
-- --------------------------------------------------------------------


data MutationType = BasicMutation | ObjectMutation      -- type of mutation, basic mutation if only basic fields are mutated 
                        deriving (Show, Eq, Ord)

type Mutation = (Int, MutationType)

verdictifyMethod :: [ConstantInfo] -> FieldDB -> MethodDB -> NamedMethodCode -> MethodType
verdictifyMethod cpool fDB mDB (mID, mCode, mCFG) = 
        let     deps = dependencies cpool mCode
                stat = getStaticFields cpool mCode                              -- static field accesses by method
                nofs = filter (\x -> (fDB ! x) /= FinalStatic) stat             -- non final static ones
                mdep = map methodID $ filter isMethod deps
                unan = filter (\x -> (mDB ! x) == UnanalyzableMethod) mdep      -- unanalyzable
                stim = filter (\x -> (mDB ! x) == StrongImpure) mdep            -- strong impure
        in      if nonEmpty unan
                then UnanalyzableMethod
                else    if nonEmpty stim
                        then StrongImpure
                        else    if nonEmpty nofs           -- accesses some non final static field
                                then Impure
                                else let mutations = sort $ getMutations cpool mDB mCFG -- indices of the parameters potentially modified / mutated
                                     in  if null mutations
                                         then Pure
                                         else if length mutations == 1 && head mutations == (0, BasicMutation)   -- modifies only this in basic fields
                                              then Local
                                              else Impure

-- | get static fields used in code
getStaticFields :: [ConstantInfo] -> [CodeAtom] -> [FieldID]
getStaticFields cpool ((_, (op : rest)) : restcode) 
        | op `elem` [getStatic, putStatic]      =     let idx = toIndex rest
                                                      in  (getFieldName cpool $ fromIntegral idx) : getStaticFields cpool restcode
        | otherwise                             =     getStaticFields cpool restcode


-- | Get all mutations done to passed parameters in code
getMutations :: [ConstantInfo] -> MethodDB -> CFG -> [Mutation]
getMutations cpool mDB cfg = undefined --stepThrough cpool mDB cfg theStart

data StackObject = SBasic | SReference Int | SReferenceFresh
                        deriving (Eq, Show)

type Stack = [StackObject] -- elements added at the beginning
type Stacks = [Stack]
type LocalHeap = M.Map Int StackObject
type LocalHeaps = [LocalHeap]

basicop :: (Int, Int) -> [Word8]        -- basicop (input stack, output stack) -> which ops
basicop (2, 1) = [96, 98..114] ++ [120, 122 .. 130] ++ [149, 150] ++ [159..164] ++ [136, 137, 142, 144]
basicop (0, 0) = [0,  132, 177, 167, 200]
basicop (0, 1) = [26..29] ++ [34..37] ++ [2..8] ++ [11..13] ++  [21, 23] ++ [16, 17]
basicop (1, 0) = [59..62] ++ [67..70] ++ [54, 56] ++ [153..158] ++ [170, 171] ++ [172, 174]
basicop (1, 1) = [116, 118] ++ [134, 139, 145, 146, 147]
basicop (0, 2) = [9, 10, 14, 15, 20, 22, 24] ++ [30..33] ++ [38..41]
basicop (2, 0) = [55, 57] ++ [63..66] ++ [71..74] ++ [173, 175]
basicop (4, 2) = [97, 99..115] ++ [121, 123 .. 131]
basicop (2, 2) = [117, 119, 138, 143]
basicop (1, 2) = [133, 135, 140, 141]
basicop (4, 1) = [148, 151, 152]

definedAt :: [(Int, Int)]
definedAt = [(2, 1), (0, 0), (0, 1), (1, 0), (1, 1), (0, 2), (2, 0), (4, 2),
                (2, 2), (1, 2), (4, 1)]

basicopAll :: [Word8]
basicopAll = concatMap basicop definedAt

-- consume objects and put stuff on stack that is basic
nonBasicop :: (Int, Int) -> [Word8]
nonBasicop (1, 1) = [193, 190]
nonBasicop (1, 0) = [198, 199, 176, 58, 87]
nonBasicop (2, 0) = [165, 166, 88]



--consumeOp :: [ConstantInfo] -> MethodDB -> CFG -> Int -> Stack -> Stacks
--consumeOp cpool mDB cfg pos = 
--        let     consumePartial = consumeOp cpool mDB cfg
--

-- Note that the list implementation for Stacks and cpos dramatically slows down code
-- However since the numbers are expected to be small we do not modify this as present
-- but must be replaced with arrays with constant time access and modifications in the
-- future.
data AnalysisBundle = AnalysisBundle {
                        cpool   :: [ConstantInfo],
                        mDB     :: MethodDB,
                        fDB     :: FieldDB,
                        cfg     :: CFG,
                        lHeaps  :: LocalHeaps,
                        stacks  :: Stacks,                              
                        cpos    :: [Int] }        deriving Show

type AnalysisM = FX AnalysisBundle MethodType   -- the analysis monad

getCpool :: AnalysisM [ConstantInfo]
getCpool = pure cpool <*> getS

getMDB :: AnalysisM MethodDB
getMDB = pure mDB <*> getS

getFDB :: AnalysisM FieldDB
getFDB = pure fDB <*> getS

getCfg :: AnalysisM CFG
getCfg = pure cfg <*> getS

getStacks :: AnalysisM Stacks
getStacks = pure stacks <*> getS

getLocalHeaps :: AnalysisM LocalHeaps
getLocalHeaps = pure lHeaps <*> getS

getCPos :: AnalysisM [Int]
getCPos = pure cpos <*> getS

setCPos :: [Int] -> AnalysisM ()
setCPos z = do
                AnalysisBundle a b c d e f _ <- getS
                putS $ AnalysisBundle a b c d e f z

setStacks :: Stacks -> AnalysisM ()
setStacks stks = do
                        AnalysisBundle a b c d e _ f <- getS
                        putS $ AnalysisBundle a b c d e stks f

setLocalHeaps :: LocalHeaps -> AnalysisM ()
setLocalHeaps lheaps = do
                        AnalysisBundle a b c d _ e f <- getS
                        putS $ AnalysisBundle a b c d lheaps e f

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs p v = take p xs ++ [v] ++ drop (p + 1) xs

stackOperate :: Stack -> (Int, Int) -> Stack
stackOperate stk (inp, outp) =  if length stk < inp 
                                then error "Stack is smaller than number of inputs required"
                                else replicate outp SBasic ++ drop inp stk
                

setStackPos :: Int -> Stack -> AnalysisM ()
setStackPos pos stk = do
                        AnalysisBundle a b c d e f g <- getS
                        putS $ AnalysisBundle a b c d e (replaceElem f pos stk) g

setLocalHeapPos :: Int -> LocalHeap -> AnalysisM ()
setLocalHeapPos pos lheap = do
                                AnalysisBundle a b c d e f g <- getS
                                putS $ AnalysisBundle a b c d (replaceElem e pos lheap) f g

consumeCode :: AnalysisM [CodeAtom] -- code bytes
consumeCode = do
                g <- getCfg
                p <- getCPos
                stk <- getStacks
                loc <- getLocalHeaps

                let q = concatMap (\(l, s, c) -> zip3 (repeat l) (repeat s) (suc g c)) $ zip3 loc stk p
                let np = map thirdof3 q

                setCPos np
                setLocalHeaps $ map firstof3 q
                setStacks $ map secondof3 q

                return $ map (nodecode . fromJust . lab g) $ np

getAnalysis :: AnalysisM MethodType
getAnalysis = do
                cas   <- consumeCode
                pos   <- getCPos

                whenExit (null pos) Pure

                stks <- getStacks
                locs <- getLocalHeaps
                
                results <- forM [0..length cas - 1] $ \j -> exceptify $ analyseAtom j (cas !! j) (locs !! j) (stks !! j)
        
                whenExit (UnanalyzableMethod `elem` results) UnanalyzableMethod
                whenExit (StrongImpure `elem` results) StrongImpure
                whenExit (Impure `elem` results) Impure
                
                getAnalysis 


newOp, getFieldOp, putFieldOp, monitorEnterOp, monitorExitOp, aThrowOp :: Word8
newOp = 187
getFieldOp = 180
putFieldOp = 181
aThrowOp   = 191

monitorEnterOp = 194
monitorExitOp = 195

analyseAtom :: Int -> CodeAtom -> LocalHeap -> Stack -> AnalysisM MethodType
analyseAtom j (pos, (op : rest)) loc stk = 
        do
                whenExit (op == monitorEnterOp || op == monitorExitOp) UnanalyzableMethod
                whenExit (op == aThrowOp) StrongImpure
        
                if (op `elem` basicopAll)
                then    let tup = head $ filter (\t -> op `elem` basicop t) definedAt
                        in  setStackPos j $ stackOperate stk tup
                else    return ()
                whenExit (op `elem` basicopAll) Pure
                
                return $ undefined
