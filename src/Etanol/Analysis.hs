{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, MultiWayIf,
  BangPatterns #-}

module Etanol.Analysis
    ( isFinalStaticField
    , dependencies
    , AnyID(..)
    , uniques
    , CPoolMap(..)
    , analyseAll
    , AnyData(..)
    , LoadedThings(..)
    , LoadedThingsStatus(..)
    , StackObject(..)
    , ReturnType(..)
    , ObjectField(..)
    , Stack(..)
    , Stacks(..)
    , MutLocs(..)
    , LocalHeap(..)
    , LocalHeaps(..)
    , Status(..)
    , AnyName(..)
    , NamePrefix(..)
    , initialStrongImpureList
    , tHRESHOLD
    , isInitialStrongImpure
    , descriptorIndices2
    , toIndex
    ) where

import Control.Applicative
import Control.Monad

-- update what is being imported
import Data.Binary (Word16, Word32, Word8)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import Data.Maybe

import ByteCodeParser.BasicTypes
import ByteCodeParser.Instructions
import ByteCodeParser.Reader
import Etanol.ControlFlowGraph
import Etanol.Decompile
import Etanol.MonadFX
import Etanol.Types

import EtanolTools.Unsafe

type NamePrefix = String -- prefixes of strings, anynames

type AnyName = String

tHRESHOLD :: Int
tHRESHOLD = 50

-- unq returns the unique elements of the list, provided they are orderable
unq = map (unsafeHead "unq") . group . sort

initialStrongImpureList :: [NamePrefix]
initialStrongImpureList = []

{--
    [ "java.io"
    , "java.net"
    , "javax.swing"
    , "java.awt"
    , "java.nio"
    , "java.sql"
    , "javax.imageio"
    , "javax.print"
    , "javax.sql"
    , "javax.rmi"
    , "javax.sound"
    ]
--}

isInitialStrongImpure :: AnyName -> Bool
isInitialStrongImpure namae = any (isPrefix namae) initialStrongImpureList

-- if s is a prefix of t
isPrefix :: String -> String -> Bool
isPrefix t s =
    if length t < length s
        then False
        else if (s == take (length s) t)
                 then True
                 else False

getFieldOp, putFieldOp, invokeStaticOp, invokeSpecialOp, invokeVirtualOp, invokeInterfaceOp, invokeDynamicOp ::
       Word8
newOp, newArrayOp, monitorEnterOp, monitorExitOp, aThrowOp, ldcOp, ldc_wOp, checkCastOp ::
       Word8
multianewArrayOp, popOp, pop2Op, dupOp, dup_x1Op, dup_x2Op, dup2Op, dup2_x1Op, dup2_x2Op, swapOp ::
       Word8
aaloadOp, iastoreOp, lastoreOp, fastoreOp, dastoreOp, aastoreOp, bastoreOp, castoreOp, sastoreOp ::
       Word8
areturnOp, wideOp :: Word8
areturnOp = 176

wideOp = 196

newOp = 187

aconstNullOp = 1

newArrayOp = 188

anewArrayOp = 189

multianewArrayOp = 197

aThrowOp = 191

monitorEnterOp = 194

monitorExitOp = 195

ldcOp = 18

ldc_wOp = 19

popOp = 87

pop2Op = 88

dupOp = 89

dup_x1Op = 90

dup_x2Op = 91

dup2Op = 92

dup2_x1Op = 93

dup2_x2Op = 94

swapOp = 95

aaloadOp = 50

iastoreOp = 79

lastoreOp = 80

fastoreOp = 81

dastoreOp = 82

aastoreOp = 83

bastoreOp = 84

castoreOp = 85

sastoreOp = 86

getFieldOp = 180

putFieldOp = 181

invokeVirtualOp = 182

invokeSpecialOp = 183

invokeStaticOp = 184

invokeInterfaceOp = 185

invokeDynamicOp = 186

checkCastOp = 192

-- When reading an index from the bytecode into the constantPool, decrement it by 1, as is done here
-- using pred
toIndex :: [Word8] -> Word32
toIndex =
    fromIntegral .
    pred .
    sum .
    map (uncurry (*)) . zip (map (256 ^) [0 ..]) . map fromIntegral . reverse

-- local heap indices do not need decrementing.
toLocalHeapIndex :: [Word8] -> Int
toLocalHeapIndex = fromIntegral . succ . toIndex

isFinalStaticField :: [ConstantInfo] -> FieldDB -> Int -> Maybe Bool
isFinalStaticField cpool fieldDB idx =
    let fid = getFieldName cpool idx
        ftype  = fieldDB !? fid
     in if isNothing ftype
            then Nothing
            else if ftype == Just Normal
                     then Just False
                     else Just True

uniques :: [AnyID] -> [AnyID]
uniques = map (unsafeHead "uniques") . group . sort

data AnyID
    = EFieldID { fieldID :: FieldID }
    | EMethodID { methodID :: MethodID }
    deriving (Eq, Ord)

instance Show AnyID where
    show (EFieldID f) = "field " ++ fst f ++ ":" ++ snd f
    show (EMethodID m) = "method " ++ fst m ++ ":" ++ snd m

data AnyData
    = EFieldData { fieldData :: NamedField }
    | EMethodData { methodData :: NamedMethodCode }
    deriving (Show, Eq)

isField :: AnyID -> Bool
isField (EFieldID _) = True
isField _ = False

isMethod :: AnyID -> Bool
isMethod (EMethodID _) = True
isMethod _ = False

-- Figure out the direct dependencies of a piece of code given the constant pool
dependencies :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
dependencies cpool codes = uniques $ depsMethod cpool codes

-- Note that this does not need to account for new or anewarray or multianewarray as
-- their constructors are called shortly after these declarations, including them in
-- the dependencies anyway
depsMethod :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
depsMethod cpool [] = []
depsMethod cpool ((idx, op:rest):restcode)
    | op `elem` [getFieldOp, putFieldOp, getStatic, putStatic] =
        let idx = toIndex rest
            fld = getFieldName cpool $ fromIntegral idx
         in [EFieldID fld] ++ depsMethod cpool restcode
    | op `elem` [invokeSpecialOp, invokeStaticOp, invokeVirtualOp] =
        let idx = toIndex rest
            mthd = getMethodName cpool $ fromIntegral idx
         in [EMethodID mthd] ++ depsMethod cpool restcode
    | otherwise = depsMethod cpool restcode

data Status
    = Analyzing
    | NotAnalyzed
    deriving (Show, Eq, Ord)

type LoadedThings = M.Map AnyID AnyData

type LoadedThingsStatus = M.Map AnyID Status

nonEmpty :: [a] -> Bool
nonEmpty = not . null

type CPoolMap = M.Map ClassName [ConstantInfo]

-- if empty list then print error else return head
unsafeHead :: String -> [a] -> a
unsafeHead err xs =
    if null xs
        then (error err)
        else (head xs)

getConstantPoolForThing :: CPoolMap -> AnyID -> [ConstantInfo]
getConstantPoolForThing cmap thing =
    if isNothing result
        then error $ "Constant Pool does not exist for " ++ cn ++ "."
        else fromJust result
  where
    toName :: AnyID -> AnyName
    toName (EFieldID f) = fst f
    toName (EMethodID m) = fst m
    cn = getClassName $ toName thing
    result = cmap !? cn

-- functions for analysis
-- | analyseAll is the main entry point for analysis.
-- | Note that by implementation, analyseAll must always return (empty map, _, _).
analyseAll ::
       CPoolMap
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB -- old  
    -> (LoadedThingsStatus, FieldDB, MethodDB) -- new values
analyseAll cmap loadedThings loadedThingsStatus fDB mDB =
    if M.null loadedThingsStatus
        then (loadedThingsStatus, fDB, mDB)
        else let (thing, _) = M.elemAt 0 loadedThingsStatus
                 cpool = getConstantPoolForThing cmap thing
                 (loadedThingsStatus', fDB', mDB') =
                     analysisDriver
                         cmap
                         cpool
                         loadedThings
                         loadedThingsStatus
                         fDB
                         mDB
                         thing
              in debugLogger
                     ("LoadedThingsStatus: " ++
                      (show $ M.size loadedThingsStatus)) $
                 analyseAll cmap loadedThings loadedThingsStatus' fDB' mDB'

toRepr :: AnyID -> String
toRepr (EFieldID f) = " Field " ++ (fst f) ++ ":" ++ (snd f)
toRepr (EMethodID m) = " Method " ++ (fst m) ++ ":" ++ (snd m)

-- assumes thing to be in loadedThings
analysisDriver ::
       CPoolMap
    -> [ConstantInfo]
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB -- olds
    -> AnyID -- target  
    -> (LoadedThingsStatus, FieldDB, MethodDB) -- new values
analysisDriver cmap cpool loadedThings loadedThingsStatus fDB mDB thing -- thing is guaranteed to be in loadedThings 
 =
    let thingdata =
            debugLogger ("Here" ++ show (loadedThings !? thing)) $
            loadedThings ! thing
     in if isField thing
            then ( M.delete thing loadedThingsStatus
                 , analyseField cpool loadedThings fDB thing
                 , mDB)
                        -- delete object                get the changed field                   old method db works
                        --                              note that we do not pass the current
                        --                              statuses to field analysis as fields are always
                        --                              analyzable in this setting
                        -- NOTE : analyseField can be called with just the cpool as it doesn't need anything else for
                        -- recursive analysis.
            else debugLogger ("Analyzing " ++ show thing) $
                 if (loadedThingsStatus ! thing) == Analyzing -- found loop
                     then let mID = methodID thing
                              mDB' = M.insert mID UnanalyzableMethod mDB
                           in (M.delete thing loadedThingsStatus, fDB, mDB')
                     else let mID = methodID thing
                              loadedThingsStatus' =
                                  M.insert thing Analyzing loadedThingsStatus
                              (loadedThingsStatus'', fDB', mDB') =
                                  analyseMethod
                                      cmap
                                      cpool
                                      loadedThings
                                      loadedThingsStatus'
                                      fDB
                                      mDB
                                      thing
                                                                        -- However you need to pass cmap here as it can recursively analyse
                                                                        -- other stuff
                                    --trace(toRepr thing ++ " => " ++ show (mDB' ! mID)) 
                           in (M.delete thing loadedThingsStatus'', fDB', mDB')

isBasic :: FieldDescriptor -> Bool
isBasic "" = error "Empty field descriptor!"
isBasic s = (head s) `elem` ['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z']

-- checks if it is final static or basic type atm, assumes AnyID is a isField
analyseField :: [ConstantInfo] -> LoadedThings -> FieldDB -> AnyID -> FieldDB
analyseField cpool loadedThings fDB thing =
    let fID = fieldID thing
        ((_, fDesc), fAccessFlags) = fieldData (loadedThings ! thing)
        verdict =
            if (AFFinal `elem` fAccessFlags) && (AFStatic `elem` fAccessFlags)
                then FinalStatic
                else if isBasic fDesc
                         then Basic
                         else Normal
     in M.insert fID verdict fDB

-- | Feeding mechanism through the analysisDriver. See `analyseMethod`
feedAll ::
       CPoolMap
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB
    -> [AnyID]
    -> (LoadedThingsStatus, FieldDB, MethodDB)
feedAll _ _ loadedThingsStatus fDB mDB [] = (loadedThingsStatus, fDB, mDB)
feedAll cmap loadedThings loadedThingsStatus fDB mDB (aID:rest) =
    let (lth', fdb', mdb') =
            if M.member aID loadedThingsStatus
                then analysisDriver
                         cmap
                         (getConstantPoolForThing cmap aID)
                         loadedThings
                         loadedThingsStatus
                         fDB
                         mDB
                         aID
                                                -- this is why you need the cmap to analyseMethod, as it can recursively call this.
                else (loadedThingsStatus, fDB, mDB)
     in feedAll cmap loadedThings lth' fdb' mdb' rest

-- checks properties for method
-- currently no recursive method is analyzed to be pure
analyseMethod ::
       CPoolMap
    -> [ConstantInfo]
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB
    -> AnyID
    -> (LoadedThingsStatus, FieldDB, MethodDB)
analyseMethod cmap cpool loadedThings loadedThingsStatus fDB mDB thing =
    let mID = methodID thing
        mName = fst mID
        mDes = snd mID
        mData = methodData (loadedThings ! thing)
        (_, mCode, mCFG, af) = mData
        deps = dependencies cpool mCode
        mDeps = filter isMethod deps
        fDeps = filter isField deps
        mNAl =
            map EMethodID $
            filter (\x -> M.notMember x mDB) $ map methodID mDeps
        fNAl =
            map EFieldID $ filter (\x -> M.notMember x fDB) $ map fieldID fDeps
        nAl = mNAl ++ fNAl
        nA = filter (\x -> M.notMember x loadedThings) nAl
                        -- not previously analyzed and also not loaded, but required for analysis
     in debugLogger
            ("Constant pool : " ++
             (if (cpool == getConstantPoolForThing cmap thing)
                  then "OK"
                  else error "Constant pool is WRONG!")) $
        if (AMNative `elem` af ||
            AMSynchronized `elem` af || null mCode) 
                        -- || AMVarargs `elem` af || null mCode) -- disabled 
                        -- VarArg check, see GSoC Phase 2 goals
                        -- These identifiers enable immediate disqualification 
                        -- or if Code is empty implying an abstract method
            then (loadedThingsStatus, fDB, M.insert mID UnanalyzableMethod mDB)
            else if isInitialStrongImpure mName
                     then ( loadedThingsStatus
                          , fDB
                          , M.insert mID StrongImpure mDB)
                     else if nonEmpty nA
                              then ( loadedThingsStatus
                                   , fDB
                                   , M.insert mID UnanalyzableMethod mDB)
                              else let (loadedThingsStatus', fDB', mDB') =
                                           feedAll
                                               cmap
                                               loadedThings
                                               loadedThingsStatus
                                               fDB
                                               mDB
                                               nAl
                                                                                -- here you call feedAll with cmap
                                        -- after this all deps of this method is not in loadedThingsStatus, as they have all been analyzed
                                        -- recursively. So we can now safely analyse this method itself
                                        -- But it may happen that this method itself got into a loop and was analyzed already
                                    in if M.member thing loadedThingsStatus'
                                           then let desidx =
                                                        descriptorIndices2 $
                                                        mDes
                                                    initialHeap =
                                                        M.fromList desidx
                                                    srefpred ::
                                                           StackObject
                                                        -> StackObject
                                                    srefpred (SReference x) =
                                                        SReference (x - 1)
                                                    srefpred y = y
                                                    lh' =
                                                        if (AMStatic `elem` af)
                                                            then M.map srefpred $
                                                                 M.mapKeys
                                                                     pred
                                                                     initialHeap
                                                                                                -- if method is static then each elem is -1
                                                                                                -- of its prev value. Experiment with descriptorIndices2
                                                                                                -- to understand
                                                            else M.insert
                                                                     0
                                                                     (SReference
                                                                          0)
                                                                     initialHeap                    -- otherwise just add the `this` instance.

                                                    replaceRefWithNull ::
                                                        StackObject ->
                                                        NStackObject
                                                    replaceRefWithNull
                                                        (SReference _) = 
                                                            NSNullable
                                                    replaceRefWithNull _ = 
                                                            NSNonNullable

                                                    lh_null = 
                                                        M.map 
                                                            replaceRefWithNull
                                                            lh'
                                                            
                                                    lhp = [lh']
                                                    lhp_null = [lh_null]
                                                    initialState =
                                                        AnalysisBundle
                                                            cpool
                                                            mDB
                                                            fDB
                                                            mCFG
                                                            lhp
                                                            [[]]
                                                            [theStart]
                                                            [[]]
                                                    (finalState, verdict) =
                                                        resultant
                                                            getAnalysis
                                                            initialState
                                                    {--
                                                    (finalState', verdict') =
                                                        resultant
                                                            getAnalysisNull
                                                            initialState_null
                                                    --}
                                                    fDB' = fieldDB finalState
                                                    mDB' = methodDB finalState
                                                    mut =
                                                        unq $
                                                        concat
                                                            (mutDB finalState)
                                                  -- if atleast one path mutates a parameter, the pure becomes local
                                                    fin =
                                                        if verdict == Pure &&
                                                           (not $ null mut)
                                                            then Local
                                                            else verdict
                                                 in ( loadedThingsStatus'
                                                    , fDB'
                                                    , M.insert mID fin mDB')
                                           else ( loadedThingsStatus'
                                                , fDB'
                                                , mDB')

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
{--
data MutationType = BasicMutation | ObjectMutation      -- type of mutation, basic mutation if only basic fields are mutated 
                        deriving (Show, Eq, Ord)

type Mutation = (Int, MutationType)

verdictifyMethod :: [ConstantInfo] 
                    -> FieldDB 
                    -> MethodDB 
                    -> NamedMethodCode 
                    -> MethodAnalysisResult
verdictifyMethod cpool fDB mDB (mID, mCode, mCFG, mAF) = 
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
--}
data StackObject
    = SBasic
    | SBasicLong
    | SReference Int
    | SReferenceFresh
    | SObjArrayReference
    deriving (Eq, Show)

data NStackObject
    = NSNullable
    | NSNonNullable
    deriving (Eq, Show)

type Stack = [StackObject] -- elements added at the beginning

type Stacks = [Stack]

type LocalHeap = M.Map Int StackObject

type LocalHeaps = [LocalHeap]

type NStack = [NStackObject]

type NStacks = [NStack]

type NLocalHeap = M.Map Int NStackObject

type NLocalHeaps = [NLocalHeap]

basicop :: (Int, Int) -> [Word8] -- basicop (input stack, output stack) -> which ops
basicop (2, 1) =
    [96,98 .. 114] ++
    [120,122 .. 130] ++ [149, 150] ++ [159 .. 164] ++ [148, 151, 152]
basicop (0, 0) = [0, 132, 177, 167, 200]
basicop (0, 1) =
    [26 .. 29] ++ [34 .. 37] ++ [2 .. 8] ++ [11 .. 13] ++ [21, 23] ++ [16, 17]
basicop (1, 0) =
    [59 .. 62] ++
    [67 .. 70] ++ [54, 56] ++ [153 .. 158] ++ [170, 171] ++ [172, 174]
basicop (1, 1) = [116, 118] ++ [134, 139, 145, 146, 147] ++ [136, 137, 142, 144]
basicop (0, 2) = []
basicop (2, 0) = [173, 175]
basicop (4, 2) = []
basicop (2, 2) = []
basicop (1, 2) = []
basicop (4, 1) = []

basicopL :: (Int, Int) -> [Word8]
basicopL (0, 1) = [9, 10, 14, 15, 20, 22, 24] ++ [30 .. 33] ++ [38 .. 41]
basicopL (1, 0) = [55, 57] ++ [63 .. 66] ++ [71 .. 74]
basicopL (2, 1) = [97,99 .. 115] ++ [121,123 .. 131]
basicopL (1, 1) = [133, 135, 140, 141, 138, 143, 117, 119]

definedAtBasic :: [(Int, Int)]
definedAtBasic =
    [ (2, 1)
    , (0, 0)
    , (0, 1)
    , (1, 0)
    , (1, 1)
    , (0, 2)
    , (2, 0)
    , (4, 2)
    , (2, 2)
    , (1, 2)
    , (4, 1)
    ]

definedAtBasicL :: [(Int, Int)]
definedAtBasicL = [(0, 1), (1, 0), (2, 1), (1, 1)]

basicopAll, basicopLAll :: [Word8]
basicopAll = concatMap basicop definedAtBasic

basicopLAll = concatMap basicopL definedAtBasicL

-- consume objects and put stuff on stack that is basic
-- also not operators that may have a non basic side effect, like iastore
nonBasicop :: (Int, Int) -> [Word8]
nonBasicop (1, 1) = [193, 190]
nonBasicop (1, 0) = [198, 199, 176, 58]
nonBasicop (2, 0) = [165, 166]
nonBasicop (2, 1) = [46, 48, 51, 52, 53]
nonBasicop (2, 2) = []

nonBasicopL (2, 1) = [47, 49]

definedAtNonBasic :: [(Int, Int)]
definedAtNonBasic = [(1, 1), (1, 0), (2, 0), (2, 1), (2, 2)]

definedAtNonBasicL :: [(Int, Int)]
definedAtNonBasicL = [(2, 1)]

nonBasicopAll, nonBasicopLAll :: [Word8]
nonBasicopAll = concatMap nonBasicop definedAtNonBasic

nonBasicopLAll = concatMap nonBasicopL definedAtNonBasicL

-- Note that the list implementation for Stacks and cpos dramatically slows down code
-- However since the numbers are expected to be small we do not modify this as present
-- but must be replaced with arrays with constant time access and modifications in the
-- future.
type MutLocs = [Int]

data AnalysisBundle = AnalysisBundle
    { cpool :: [ConstantInfo]
    , methodDB :: MethodDB
    , fieldDB :: FieldDB
    , cfg :: CFG
    , lHeaps :: LocalHeaps
    , stacks :: Stacks
    , cpos :: [Int]
    , mutDB :: [MutLocs]
    } deriving (Show)

type AnalysisM = FX AnalysisBundle MethodType -- the analysis monad

getCpool :: AnalysisM [ConstantInfo]
getCpool = pure cpool <*> getS

getMDB :: AnalysisM MethodDB
getMDB = pure methodDB <*> getS

getFDB :: AnalysisM FieldDB
getFDB = pure fieldDB <*> getS

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
    AnalysisBundle a b c d e f _ g <- getS
    putS $ AnalysisBundle a b c d e f z g

setStacks :: Stacks -> AnalysisM ()
setStacks stks = do
    AnalysisBundle a b c d e _ f g <- getS
    putS $ AnalysisBundle a b c d e stks f g

setLocalHeaps :: LocalHeaps -> AnalysisM ()
setLocalHeaps lheaps = do
    AnalysisBundle a b c d _ f g h <- getS
    putS $ AnalysisBundle a b c d lheaps f g h

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs p v = take p xs ++ [v] ++ drop (p + 1) xs

-- remove anything but add short basic things
stackOperate :: Stack -> (Int, Int) -> Stack
stackOperate stk (inp, outp) =
    if length stk < inp
        then error "Stack is smaller than number of inputs required"
        else replicate outp SBasic ++ drop inp stk

-- remove anything but adds long things
stackOperateL :: Stack -> (Int, Int) -> Stack
stackOperateL stk (inp, outp) =
    if length stk < inp
        then error "Stack is smaller than number of inputs required"
        else replicate outp SBasicLong ++ drop inp stk

setStackPos :: Int -> Stack -> AnalysisM ()
setStackPos pos stk = do
    AnalysisBundle a b c d e f g h <- getS
    putS $ AnalysisBundle a b c d e (replaceElem f pos stk) g h

setLocalHeapPos :: Int -> LocalHeap -> AnalysisM ()
setLocalHeapPos pos lheap = do
    AnalysisBundle a b c d e f g h <- getS
    putS $ AnalysisBundle a b c d (replaceElem e pos lheap) f g h

getLocalHeapElem :: LocalHeap -> Int -> StackObject
getLocalHeapElem loc pos = loc ! pos -- error if does not exist

localHeapOperate :: LocalHeap -> Int -> StackObject -> LocalHeap
localHeapOperate loc pos obj = M.insert pos obj loc

getMuts :: AnalysisM [MutLocs]
getMuts = pure mutDB <*> getS

setMuts :: [MutLocs] -> AnalysisM ()
setMuts ms = do
    AnalysisBundle a b c d e f g _ <- getS
    putS $ AnalysisBundle a b c d e f g ms

-- add a mutation location to a specific instance
addMut :: Int -> Int -> AnalysisM ()
addMut pos v = do
    mutl <- getMuts
    setMuts $ replaceElem mutl pos (v : (mutl !! pos))

consumeCode :: AnalysisM [CodeAtom] -- code bytes
consumeCode = do
    g <- getCfg
    p <- getCPos
    stk <- getStacks
    loc <- getLocalHeaps
    mut <- getMuts
    let q =
            concatMap
                (\(l, s, c, m) ->
                     zip4 (repeat l) (repeat s) (suc g c) (repeat m)) $
            zip4 loc stk p mut
    let np = map thirdof4 q
    setCPos np
    setLocalHeaps $ map firstof4 q
    setStacks $ map secondof4 q
    setMuts $ map fourthof4 q
    return $ map (nodecode . fromJust . lab g) $ np

getAnalysis :: AnalysisM MethodType
getAnalysis = do
    cas <- consumeCode
    pos <- getCPos
    whenExit (all (== theEnd) pos) Pure -- all of them are at the end
    stks <- getStacks
    locs <- getLocalHeaps
    results <-
        forM [0 .. length cas - 1] $ \j ->
            exceptify $ analyseAtom j (cas !! j) (locs !! j) (stks !! j)
    mutl <- getMuts
    whenExit (length results > tHRESHOLD) UnanalyzableMethod -- too many branches
    whenExit (UnanalyzableMethod `elem` results) UnanalyzableMethod
    whenExit (StrongImpure `elem` results) StrongImpure
    whenExit (Impure `elem` results) Impure
    getAnalysis

-- position of this in the param list
thisPos :: Int
thisPos = 0

loads1, loads2, stores1, stores2, loadsi, storesi, loads, stores :: [Word8] -- can potentially do type changing local heap modifications
loads1 = [21, 23, 25] ++ [26 .. 29] ++ [34 .. 37] ++ [42 .. 45]

loads2 = [21 .. 45] \\ loads1

loadsi = [21 .. 25] -- indexed

loads = loads1 ++ loads2 ++ loadsi

stores1 = [54, 56, 58] ++ [59 .. 62] ++ [67 .. 70] ++ [75 .. 78]

stores2 = [54 .. 78] \\ stores1

storesi = [54 .. 58] -- indexed

stores = stores1 ++ stores2 ++ storesi

getIdxOp :: [Word8] -> Int
getIdxOp (op:rest) =
    if op `elem` (loadsi ++ storesi)
        then toLocalHeapIndex rest
        else if | op `elem` [26, 30, 34, 38, 42, 59, 63, 67, 71, 75] -> 0
                | op `elem` [27, 31, 35, 39, 43, 60, 64, 68, 72, 76] -> 1
                | op `elem` [28, 32, 36, 40, 44, 61, 65, 69, 73, 77] -> 2
                | op `elem` [29, 33, 37, 41, 45, 62, 66, 70, 74, 78] -> 3
                | otherwise ->
                    error $
                    "Invalid operator for loading / storing : " ++ show op

{--
The current analysis uses the following strategy.
For a method to be analyzable it cannot be recursive. --- Important!
"""""""""""""""""""""""""""""""" cannot use monitor* ops
"""""""""""""""""""""""""""""""" cannot use varargs -- not implemented yet!
If it calls anything from initialStrongImpureList transitively, then it is strong impure.
If it uses exceptions, it is strong impure.
If it assigns Reference type fields to any object/array (fresh or parameter), it is impure. -- refers to putfield
=> All accessed object fields are fresh. -- refers to getfield (only Fresh objects or basic objects put into stack on calls to getField)
Otherwise it can be impure, local or pure.
Suppose basic fields are assigned to some parameters.
If any parameter other than 0 are assigned then it is impure (0 == this)
If only 0 is assigned it is local.
Otherwise it is pure.

Local and pure method may only return Fresh References or BasicTypes otherwise, they become pure, see (*****)

Also if pure functions return references, they must be fresh and not any of the referenced params
--}

analyseAtom :: Int -> CodeAtom -> LocalHeap -> Stack -> AnalysisM MethodType
analyseAtom j (pos, []) loc stk = return Pure
analyseAtom j (pos, ca@(op:rest)) loc stk
                --traceM $ "Code : " ++ show pos ++ " : " ++ show ca
 = do
    whenExit
        (op == monitorEnterOp ||
         op == monitorExitOp || op == invokeInterfaceOp || op == invokeDynamicOp)
        UnanalyzableMethod
                -- NOTE : The below restriction is totally lame, is due to the laziness of the author. Will be fixed asap.
    whenExit (op == wideOp) UnanalyzableMethod
    whenExit (op == aThrowOp) StrongImpure
    cpool <- getCpool
    fDB <- getFDB
    mDB <- getMDB
                -- (*****) - reference from above comment
    whenExit
        (op == areturnOp && (isJust $ isParamRef $ unsafeHead "Return ops" stk))
        Impure -- returns a reference to something passed in as an argument
    when (op `elem` loads) $ do
        let idx = getIdxOp ca
        if op `elem` loads1
            then let elm = getLocalHeapElem loc idx
                  in setStackPos j $ (elm : stk)
            else let elm = getLocalHeapElem loc idx -- push long, but the thing is the same;  elm is SBasicLong
                  in setStackPos j $ (elm : stk)
    when (op `elem` stores) $ do
        let idx = getIdxOp ca
        if op `elem` stores1
            then let elm = unsafeHead "Stores if 1" stk
                  in do setLocalHeapPos j $ localHeapOperate loc idx elm
                        setStackPos j $ stackOperate stk (1, 0) -- remove 1
            else let elm = unsafeHead "Stores else 1" stk
                  in do setLocalHeapPos j $ localHeapOperate loc idx elm
                        setStackPos j $ stackOperateL stk (1, 0) -- remove 1 only, but now elm is SBasicLong
    if elem op $ basicopAll \\ (loads ++ stores) -- do everything basic that does not involve the local heap
        then let tup =
                     unsafeHead "1" $
                     filter (\t -> op `elem` basicop t) definedAtBasic
              in setStackPos j $ stackOperate stk tup
        else return ()
    if elem op $ basicopLAll \\ (loads ++ stores) -- do everything basic that does not involve the local heap; special for long
        then let tup =
                     unsafeHead "2" $
                     filter (\t -> op `elem` basicopL t) definedAtBasicL
              in setStackPos j $ stackOperateL stk tup
        else return ()
    if elem op $ nonBasicopAll \\ (loads ++ stores) -- see defn for nonBasicop
        then let tup =
                     unsafeHead "3" $
                     filter (\t -> op `elem` nonBasicop t) definedAtNonBasic
              in setStackPos j $ stackOperate stk tup
        else return ()
    if elem op $ nonBasicopLAll \\ (loads ++ stores) -- see defn for nonBasicop; special for long ops
        then let tup =
                     unsafeHead "4" $
                     filter (\t -> op `elem` nonBasicopL t) definedAtNonBasicL
              in setStackPos j $ stackOperateL stk tup
        else return ()
                {- Following this point, everything deals with operands specifically. See the brief spec above. -}
    when (op == newOp || op == newArrayOp) $ do
        setStackPos j $ SReferenceFresh : stk -- push fresh reference
    when (op == anewArrayOp || op == multianewArrayOp) $ do
        setStackPos j $ SObjArrayReference : stk -- push new obj array ref
    when (op == aconstNullOp) $ do
        setStackPos j $ SReferenceFresh : stk -- push fresh reference for null
                -- note that the following analysis is performed above as well, but it will be removed
                -- in favour of fully monadic processing.
    whenExit
        ((op == getStatic || op == putStatic) &&
         getSTypeOfFieldWord8 rest cpool == OFReference)
        Impure
    whenExit
        ((op == getStatic || op == putStatic) &&
         (isNothing $ isFinalStaticFieldWord8 rest cpool fDB))
        UnanalyzableMethod
    whenExit
        ((op == getStatic || op == putStatic) &&
         (not $ isFinalStaticFieldWord8MaybeStripped rest cpool fDB))
        Impure
    when (op == getStatic) $ do
        let ty = getSTypeOfFieldWord8 rest cpool
        if | ty == OFBasic -> setStackPos j (SBasic : stk)
           | ty == OFBasicLong -> setStackPos j (SBasicLong : stk)
           | otherwise ->
               error
                   "Unexpected exception. getStatic does not expect a Reference"
                -- when (op == putStatic) $ error "putStatic not expected!"
                -- This line was removed because the above conditions do not guarantee that putStatic does not occur
                -- the only thing that is guaranteed is that putStatic occurs on a FinalStatic field, and that can occur
                -- only in <clinit>. We believe that does not cause much of a problem.
    when (op == getFieldOp) $ do
        let ftype = getSTypeOfFieldWord8 rest cpool
            topelem = unsafeHead "getFieldOp" stk
            rstk = tail stk
        if | ftype == OFBasic -> setStackPos j (SBasic : rstk)
           | ftype == OFBasicLong -> setStackPos j (SBasicLong : rstk)
           | otherwise -> setStackPos j (topelem : rstk)
                                                                                        --               SReference Int then this is also int SReference
    when (op == putFieldOp) $ do
        let ftype = getSTypeOfFieldWord8 rest cpool
            !obj = stk !! 1 -- item 2    
        if ftype == OFReference
            then exitWith Impure
            else do
                setStackPos j $ stackOperate stk (2, 0)
                let ty = isParamRef obj
                if isJust ty
                    then addMut j $ fromJust ty -- add to parameter mutations
                    else return () -- else modifies fresh param
                {- Analysis for the whole suite of pops, dups and also swap -}
    when (op == popOp) $ setStackPos j $ tail stk
    when (op == pop2Op) $ setStackPos j $ tail $ tail stk
    when (op == dupOp) $ do
        let ~(e1:rest) = stk
        setStackPos j $ [e1, e1] ++ rest
    when (op == dup_x1Op) $ do
        let ~(e1:(e2:rest)) = stk
        setStackPos j $ [e1, e2, e1] ++ rest
    when (op == dup_x2Op) $ do
        let ~(e1:(e2:rest)) = stk
        if isCat2 e2
            then setStackPos j $ [e1, e2, e1] ++ rest
            else let (e3:rest') = rest
                  in setStackPos j $ [e1, e2, e3, e1] ++ rest'
    when (op == dup2Op) $ do
        let ~(e1:rest) = stk
        if isCat2 e1
            then setStackPos j $ [e1, e1] ++ rest
            else let ~(e2:rest) = stk
                  in setStackPos j $ [e1, e2, e1, e2] ++ rest
    when (op == dup2_x1Op) $ do
        let ~(e1:(e2:rest)) = stk
        if isCat2 e1
            then setStackPos j $ [e1, e2, e1] ++ rest
            else let ~(e3:rest') = stk
                  in setStackPos j $ [e1, e2, e3, e1, e2] ++ rest'
    when (op == dup2_x2Op) $ do
        let ~(e1:(e2:(e3:rest))) = stk
        if isCat2 e1
            then setStackPos j $ [e1, e2, e3, e1] ++ rest
            else let ~(e4:rest') = rest
                  in setStackPos j $ [e1, e2, e3, e4, e1, e2] ++ rest'
    when (op == swapOp) $ do
        let ~(e1:(e2:rest)) = stk
        setStackPos j $ [e2, e1] ++ rest
                {- dups, swaps, pops complete -}
    when (op == checkCastOp) $ return ()
    when (op == ldcOp || op == ldc_wOp) $ do
        let ftype = getSTypeOfConstantWord8 rest cpool
        if | ftype == OFBasic -> setStackPos j $ (SBasic : stk)
           | ftype == OFBasicLong -> setStackPos j $ (SBasicLong : stk)
           | ftype == OFReference -> setStackPos j $ (SReferenceFresh : stk)
           | otherwise -> exitWith UnanalyzableMethod
    when (op == aaloadOp) $ do
        let ~(_:(e1:rest)) = stk
        if | e1 == SReferenceFresh -> setStackPos j $ (SReferenceFresh : rest)
           | otherwise -> setStackPos j $ (e1 : rest)
    when
        (op == iastoreOp ||
         op == fastoreOp || op == lastoreOp || op == dastoreOp) $ do
        let ~(_:(_:(e1:rest))) = stk
            ty = isParamRef e1
        if isJust ty
            then setStackPos j rest
            else setStackPos j rest
    when (op == aastoreOp) $ do
        let ~(_:(_:(e1:rest))) = stk
            ty = isParamRef e1
        setStackPos j rest
        if isJust ty
            then exitWith Impure
            else return ()
    when (op == invokeVirtualOp || op == invokeSpecialOp) $ do
        let mID = getMethodName cpool (fromIntegral $ toIndex rest)
            nar = length $ descriptorIndices $ snd mID
            ~(obj:argsrem) = stk
            args = take nar argsrem
            oth = drop nar argsrem
            par = filter (isJust . isParamRef) (obj : args) -- parameters of this function sent in as parameter to that function 
            mty = mDB !? mID
        if isNothing mty
            then exitWith UnanalyzableMethod
            else do
                let mt = fromJust mty
                if | mt == Impure -> exitWith Impure
                   | mt == Local ->
                       do let retty = getReturnType mID
                          if | retty == RTReference ->
                                 setStackPos j $ SReferenceFresh : oth
                             | retty == RTBasic -> setStackPos j $ SBasic : oth
                             | retty == RTBasicLong ->
                                 setStackPos j $ SBasicLong : oth
                             | retty == RTVoid -> setStackPos j $ oth
                             | otherwise ->
                                 error "Unknown return type from method!"
                          mapM_ (addMut j) $ map unParamRef par -- modified params changed
                   | mt == StrongImpure -> exitWith StrongImpure
                   | mt == UnanalyzableMethod -> exitWith UnanalyzableMethod
                   | otherwise ->
                       let retty = getReturnType mID
                        in if | retty == RTReference ->
                                  setStackPos j $ SReferenceFresh : oth
                              | retty == RTBasic -> setStackPos j $ SBasic : oth
                              | retty == RTBasicLong ->
                                  setStackPos j $ SBasicLong : oth
                              | retty == RTVoid -> setStackPos j $ oth
                              | otherwise ->
                                  error "Unknown return type from method!"
    when (op == invokeStaticOp) $ do
        let mID = getMethodName cpool (fromIntegral $ toIndex rest)
            nar = length $ descriptorIndices $ snd mID
            argsrem = stk
            args = take nar argsrem
            oth = drop nar argsrem
            par = filter (isJust . isParamRef) args -- parameters of this function sent in as parameter to that function 
            mty = mDB !? mID
        if isNothing mty
            then exitWith UnanalyzableMethod
            else do
                let mt = fromJust mty
                if | mt == Impure -> exitWith Impure
                   | mt == Local ->
                       do let retty = getReturnType mID
                          if | retty == RTReference ->
                                 setStackPos j $ SReferenceFresh : oth
                             | retty == RTBasic -> setStackPos j $ SBasic : oth
                             | retty == RTBasicLong ->
                                 setStackPos j $ SBasicLong : oth
                             | retty == RTVoid -> setStackPos j $ oth
                             | otherwise ->
                                 error "Unknown return type from method!"
                          mapM_ (addMut j) $ map unParamRef par -- modified params changed
                   | mt == StrongImpure -> exitWith StrongImpure
                   | mt == UnanalyzableMethod -> exitWith UnanalyzableMethod
                   | otherwise ->
                       let retty = getReturnType mID
                        in if | retty == RTReference ->
                                  setStackPos j $ SReferenceFresh : oth
                              | retty == RTBasic -> setStackPos j $ SBasic : oth
                              | retty == RTBasicLong ->
                                  setStackPos j $ SBasicLong : oth
                              | retty == RTVoid -> setStackPos j oth
                              | otherwise ->
                                  error "Unknown return type from method!"
    return Pure

data ObjectField
    = OFBasic
    | OFBasicLong
    | OFReference
    | OFUnknown
    deriving (Show, Eq)

data ReturnType
    = RTBasic
    | RTBasicLong
    | RTReference
    | RTVoid
    deriving (Show, Eq)

basicList, longList, refList, voidElem :: [Char]
basicList = "BCFISZ"

longList = "DJ"

refList = "L["

voidElem = "V"

getReturnType :: MethodID -> ReturnType
getReturnType (_, des) =
    let ret = reverse $ takeWhile (/= ')') $ reverse des
        fc = unsafeHead "getReturnType" ret
     in if | fc `elem` basicList -> RTBasic
           | fc `elem` longList -> RTBasicLong
           | fc `elem` refList -> RTReference
           | fc `elem` voidElem -> RTVoid
           | otherwise ->
               error $
               "Unknown return type for method with descriptor : " ++ show des

isCat2 :: StackObject -> Bool
isCat2 SBasicLong = True
isCat2 _ = False

unParamRef :: StackObject -> Int
unParamRef (SReference x) = x
unParamRef _ = error "unParamRef called on non SReference type"

isParamRef :: StackObject -> Maybe Int
isParamRef (SReference x) = Just x
isParamRef _ = Nothing

getSTypeOfConstantWord8 :: [Word8] -> [ConstantInfo] -> ObjectField
getSTypeOfConstantWord8 rest cpool =
    let idx = fromIntegral $ toIndex rest
        vp = constType $ cpool !@ idx
     in case vp of
            CClass -> OFReference
            CInteger -> OFBasic
            CLong -> OFBasicLong
            CFloat -> OFBasic
            CString -> OFReference
            CDouble -> OFBasicLong
            _ -> OFUnknown

isFinalStaticFieldWord8MaybeStripped ::
       [Word8] -> [ConstantInfo] -> FieldDB -> Bool
isFinalStaticFieldWord8MaybeStripped a b c =
    (\m ->
         if isJust m
             then fromJust m
             else False) $
    isFinalStaticFieldWord8 a b c

isFinalStaticFieldWord8 :: [Word8] -> [ConstantInfo] -> FieldDB -> Maybe Bool
isFinalStaticFieldWord8 rest cpool fdb =
    let idx = toIndex rest
        fID = getFieldName cpool (fromIntegral idx)
        fty = fdb !? fID
     in if isJust fty
            then Just $ fromJust fty == FinalStatic
            else Nothing

getSTypeOfFieldWord8 :: [Word8] -> [ConstantInfo] -> ObjectField
getSTypeOfFieldWord8 rest cpool =
    getSTypeOfField (fromIntegral $ toIndex rest) cpool

getSTypeOfField :: Int -> [ConstantInfo] -> ObjectField
getSTypeOfField idx cpool =
    let des = unsafeHead "getSType" $ snd $ getFieldName cpool idx
     in if | des `elem` basicList -> OFBasic
           | des `elem` longList -> OFBasicLong
           | des `elem` refList -> OFReference
           | otherwise -> error $ "Invalid Field Type of character " ++ show des

descriptorIndices2 :: String -> [(Int, StackObject)]
descriptorIndices2 descriptor = recursiveCalc desc2 1
  where
    desc2 = takeWhile (')' /=) $ drop 1 descriptor -- Convert (xxx)yyy -> xxx
    recursiveCalc :: String -> Int -> [(Int, StackObject)]
    recursiveCalc ("") x = []
    recursiveCalc (c:left) x =
        if c `elem` (basicList ++ longList)
            then if c `elem` longList
                     then (x, SBasicLong) : recursiveCalc left (x + 2)
                     else (x, SBasic) : recursiveCalc left (x + 1)
            else if c == 'L'
                     then (x, SReference x) :
                          recursiveCalc
                              (drop 1 $ dropWhile (';' /=) left)
                              (x + 1)
                     else if c == '['
                              then let arrTypeAnd = dropWhile ('[' ==) left
                                       arrType =
                                           unsafeHead
                                               "descriptorIndices2"
                                               arrTypeAnd
                                    in if arrType == 'L'
                                           then (x, SReference x) :
                                                recursiveCalc
                                                    (drop 1 $
                                                     dropWhile
                                                         (';' /=)
                                                         arrTypeAnd)
                                                    (x + 1)
                                           else (x, SReference x) :
                                                recursiveCalc
                                                    (drop 1 arrTypeAnd)
                                                    (x + 1)
                              else []
