{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, MultiWayIf,
  BangPatterns, ViewPatterns #-}

module Etanol.Analysis
    ( isFinalStaticField
    , dependencies
    , AnyID(..)
    , uniques
    , CPoolMap(..)
    , CPoolProvider(..)
    , DepsNotFound(..)
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
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Vector as V
--import Data.Vector ((!))

import ByteCodeParser.BasicTypes
import ByteCodeParser.Instructions
import ByteCodeParser.Reader
import Etanol.ControlFlowGraph
import Etanol.Decompile
import Etanol.MonadFX
import Etanol.Types
import Etanol.Utils (weirdClass)

import qualified Data.ByteString.Lazy as BL

import EtanolTools.Unsafe

type DepsNotFound = [AnyID]

type NamePrefix = T.Text -- prefixes of strings, anynames

type AnyName = T.Text

type CPoolProvider = ClassName -> Maybe (V.Vector ConstantInfo)
                                                       -- provider of constant
                                                       -- pools

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
isInitialStrongImpure !namae = any (\s -> T.isPrefixOf s namae) initialStrongImpureList

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
toIndex :: BL.ByteString -> Word32
toIndex xs =
    fromIntegral $!
    pred $!
    sum $!
    map (uncurry (*)) $! zip (map (256 ^) [0 ..]) $! map fromIntegral $! reverse $! BL.unpack xs

-- local heap indices do not need decrementing.
toLocalHeapIndex :: BL.ByteString -> Int
toLocalHeapIndex xs = fromIntegral $! succ $! toIndex xs

isFinalStaticField :: V.Vector ConstantInfo -> FieldDB -> Int -> Maybe Bool
isFinalStaticField !cpool fieldDB !idx =
    let !fid = getFieldName cpool idx
        !ftype  = fieldDB !? fid
     in if isNothing ftype
            then Nothing
            else if ftype == Just Normal
                     then Just False
                     else Just True

uniques :: [AnyID] -> [AnyID]
uniques = map (unsafeHead "uniques") . group . sort

data AnyData
    = EFieldData { fieldData :: !NamedField }
    | EMethodData { methodData :: !NamedMethodCode }
    deriving (Show, Eq)

isField :: AnyID -> Bool
isField (EFieldID _) = True
isField _ = False

isMethod :: AnyID -> Bool
isMethod (EMethodID _) = True
isMethod _ = False

-- Figure out the direct dependencies of a piece of code given the constant pool
dependencies :: V.Vector ConstantInfo -> V.Vector CodeAtom -> [AnyID]
dependencies cpool !codes = uniques $! depsMethod cpool codes

-- Note that this does not need to account for new or anewarray or multianewarray as
-- their constructors are called shortly after these declarations, including them in
-- the dependencies anyway
depsMethod :: V.Vector ConstantInfo -> V.Vector CodeAtom -> [AnyID]
depsMethod cpool vca
    | vca == V.empty    = []         ---((idx, op:rest):restcode)
    | otherwise         =
        if  | op `elem` [getFieldOp, putFieldOp, getStatic, putStatic] ->
                let !idx = toIndex rest
                    !fld = getFieldName cpool $ fromIntegral idx
                 in (EFieldID fld) : (depsMethod cpool restcode)
            | op `elem` [invokeSpecialOp, invokeStaticOp, invokeVirtualOp] ->
                let !idx = toIndex rest
                    !mthd = getMethodName cpool $ fromIntegral idx
                 in (if not (isInterfaceMethod cpool (fromIntegral idx))
                    then ((EMethodID mthd) : )
                    else id) $ depsMethod cpool restcode    -- do not include interface methods
            | otherwise     -> depsMethod cpool restcode

            where (e, bs)  = V.head vca
                  restcode = V.tail vca
                  op       = BL.head bs
                  rest     = BL.tail bs
data Status
    = Analyzing
    | NotAnalyzed
    deriving (Show, Eq, Ord)

type LoadedThings = M.Map AnyID AnyData

type LoadedThingsStatus = M.Map AnyID Status

nonEmpty :: [a] -> Bool
nonEmpty = not . null

type CPoolMap = M.Map ClassName (V.Vector ConstantInfo)

-- if empty list then print error else return head
unsafeHead :: String -> [a] -> a
unsafeHead !err xs =
    if null xs
        then (error err)
        else (head xs)

getConstantPoolForThing :: CPoolProvider -> AnyID -> (V.Vector ConstantInfo)
getConstantPoolForThing !cpp !thing =
    if isNothing result
        then error $ "Constant Pool does not exist for " ++ T.unpack cn ++ "."
        else fromJust result
  where
    toName :: AnyID -> AnyName
    toName (EFieldID f) = fst f
    toName (EMethodID m) = fst m
    cn = getClassName $! toName thing
    result = cpp cn

-- functions for analysis
-- | analyseAll is the main entry point for analysis.
-- | Note that by implementation, analyseAll must always return (empty map, _, _).
analyseAll ::
       CPoolProvider
    -> S.Set ClassName
    -> LoadedThings
    -> LoadedThingsStatus
    -> [AnyID]
    -> FieldDB
    -> MethodDB
    -> FieldNullabilityDB
    -> MethodNullabilityDB
    -> Either DepsNotFound (LoadedThingsStatus, FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB) -- new values
analyseAll !cpp classes !loadedThings !loadedThingsStatus !focus !fDB !mDB !n_fDB !n_mDB =
    --if M.null loadedThingsStatus
      if null focus  
        then Right (loadedThingsStatus, fDB, mDB, n_fDB, n_mDB)
        else --let (!thing, _) = M.elemAt 0 loadedThingsStatus
             let (thing : rest) = focus
                 cpool = getConstantPoolForThing cpp thing
                 
                 result =     
                    analysisDriver
                         cpp
                         cpool
                         classes
                         loadedThings
                         loadedThingsStatus
                         fDB
                         mDB
                         n_fDB
                         n_mDB
                         thing
              in case result of 
                    Left dnf    -> Left dnf
                    Right (!loadedThingsStatus', !fDB', !mDB', !n_fDB', !n_mDB') ->                 
                            debugLogger
                                ("LoadedThingsStatus: " ++
                                    (show $! M.size loadedThingsStatus)) $
                                analyseAll cpp classes loadedThings loadedThingsStatus' rest fDB' mDB' n_fDB' n_mDB'

toRepr :: AnyID -> String
toRepr (EFieldID f) = " Field " ++ T.unpack (fst f) ++ ":" ++ T.unpack (snd f)
toRepr (EMethodID m) = " Method " ++ T.unpack (fst m) ++ ":" ++ T.unpack (snd m)

-- assumes thing to be in loadedThings
analysisDriver ::
       CPoolProvider
    -> V.Vector ConstantInfo
    -> S.Set ClassName
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB -- olds
    -> FieldNullabilityDB
    -> MethodNullabilityDB
    -> AnyID -- target
    -> Either DepsNotFound (LoadedThingsStatus, FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB) -- new values
analysisDriver cpp !cpool classes !loadedThings !loadedThingsStatus !fDB !mDB !n_fDB !n_mDB !thing -- thing is guaranteed to be in loadedThings
 =
    let thingdata =
            debugLogger ("Here" ++ show (loadedThings !? thing)) $
            loadedThings ! thing
     in if isField thing
            then Right $ 
                    ( M.delete thing loadedThingsStatus
                    , analyseField cpool loadedThings fDB thing
                    , mDB
                    , analyseField_null cpool loadedThings n_fDB thing
                    , n_mDB)
                        -- delete object                get the changed field                   old method db works
                        --                              note that we do not pass the current
                        --                              statuses to field analysis as fields are always
                        --                              analyzable in this setting
                        -- NOTE : analyseField can be called with just the cpool as it doesn't need anything else for
                        -- recursive analysis.
            else debugLogger ("Analyzing " ++ show thing) $
                 if (loadedThingsStatus ! thing) == Analyzing -- found loop
                     then let !mID = methodID thing
                              !mDB' = M.insert mID UnanalyzableMethod mDB
                              !n_mDB' = M.insert mID UnanalyzableNullMethod n_mDB
                           in Right (M.delete thing loadedThingsStatus, fDB, mDB', n_fDB, n_mDB')
                     else let !mID = methodID thing
                              !loadedThingsStatus' =
                                  M.insert thing Analyzing loadedThingsStatus
                                  
                              result = 
                                    analyseMethod
                                      cpp
                                      cpool
                                      classes
                                      loadedThings
                                      loadedThingsStatus'
                                      fDB
                                      mDB
                                      n_fDB
                                      n_mDB
                                      thing
                                                                        -- However you need to pass cmap here as it can recursively analyse
                                                                        -- other stuff
                                    --trace(toRepr thing ++ " => " ++ show (mDB' ! mID))
                           in case result of
                                Left dnf        -> Left dnf
                                Right (!loadedThingsStatus'', !fDB', !mDB', !n_fDB', !n_mDB') 
                                                -> Right (M.delete thing loadedThingsStatus'', fDB', mDB', n_fDB', n_mDB')

isBasic :: FieldDescriptor -> Bool
isBasic "" = error "Empty field descriptor!"
isBasic s = (T.head s) `elem` ['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z']

-- checks if it is final static or basic type atm, assumes AnyID is a isField
analyseField :: V.Vector ConstantInfo -> LoadedThings -> FieldDB -> AnyID -> FieldDB
analyseField cpool !loadedThings !fDB !thing =
    let !fID = fieldID thing
        ((_, !fDesc), !fAccessFlags) = fieldData (loadedThings ! thing)
        verdict =
            if (AFFinal `elem` fAccessFlags) && (AFStatic `elem` fAccessFlags)
                then FinalStatic
                else if isBasic fDesc
                         then Basic
                         else Normal
     in M.insert fID verdict fDB

analyseField_null :: V.Vector ConstantInfo -> LoadedThings -> FieldNullabilityDB -> AnyID -> FieldNullabilityDB
analyseField_null !cpool !loadedThings !n_fDB !thing =
    let fID = fieldID thing
        ((_, !fDesc), !fAccessFlags) = fieldData (loadedThings ! thing)
        verdict =
            if (AFFinal `elem` fAccessFlags) && (AFStatic `elem` fAccessFlags)
                then NonNullableField       -- first approximation,
                                            -- final static reference fields
                                            -- are not necessarily nonnull
                                            -- but this is currently used.
                else if isBasic fDesc
                         then NonNullableField  -- basic fields are nonnull
                         else NullableField     -- otherwise for safety mark
                                                -- nullable
     in M.insert fID verdict n_fDB


-- | Feeding mechanism through the analysisDriver. See `analyseMethod`
feedAll ::
       CPoolProvider
    -> LoadedThings
    -> S.Set ClassName
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB
    -> FieldNullabilityDB
    -> MethodNullabilityDB
    -> [AnyID]
    -> Either DepsNotFound (LoadedThingsStatus, FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB)
feedAll _ _ _ loadedThingsStatus fDB mDB n_fDB n_mDB [] = Right (loadedThingsStatus, fDB, mDB, n_fDB, n_mDB)
feedAll cpp !loadedThings classes !loadedThingsStatus !fDB !mDB !n_fDB !n_mDB (aID:rest) =
    let result =
            if M.member aID loadedThingsStatus
                then analysisDriver
                         cpp
                         (getConstantPoolForThing cpp aID)
                         classes 
                         loadedThings
                         loadedThingsStatus
                         fDB
                         mDB
                         n_fDB
                         n_mDB
                         aID
                                                -- this is why you need the cmap to analyseMethod, as it can recursively call this.
                else Right $ (loadedThingsStatus, fDB, mDB, n_fDB, n_mDB)
     in case result of 
            Left dnf -> Left dnf
            Right (!lth', !fdb', !mdb', !n_fdb', !n_mdb') -> feedAll cpp loadedThings classes lth' fdb' mdb' n_fdb' n_mdb' rest

-- checks properties for method
-- currently no recursive method is analyzed to be pure
analyseMethod ::
       CPoolProvider
    -> V.Vector ConstantInfo
    -> S.Set ClassName
    -> LoadedThings
    -> LoadedThingsStatus
    -> FieldDB
    -> MethodDB
    -> FieldNullabilityDB
    -> MethodNullabilityDB
    -> AnyID
    -> Either DepsNotFound (LoadedThingsStatus, FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB)
analyseMethod cpp !cpool classes !loadedThings !loadedThingsStatus !fDB !mDB !n_fDB !n_mDB !thing =
    let !mID = methodID thing
        !mName = fst mID
        !mDes = snd mID
        !mData = methodData (loadedThings ! thing)
        (_, !mCode, !mCFG, !af) = mData
        !deps = dependencies cpool mCode
        !mDeps = filter isMethod deps
        !fDeps = filter isField deps
        !mNAl =
            map EMethodID $!
            filter (\x -> M.notMember x mDB) $! map methodID mDeps
        !fNAl =
            map EFieldID $! filter (\x -> M.notMember x fDB) $! map fieldID fDeps
        !nAl = mNAl ++ fNAl
        !nA'' = filter (\x -> M.notMember x loadedThings) nAl
        nA' = filter (\x -> S.notMember (anyIDToClassName x) classes) nA''
        nA = filter (not . weirdClass . anyIDToClassName) nA'
                        -- not previously analyzed and also not loaded, but required for analysis
     in debugLogger
            ("Constant pool : " ++
             (if (cpool == getConstantPoolForThing cpp thing)
                  then "OK"
                  else error "Constant pool is WRONG!")) $!
        if (AMNative `elem` af ||
            AMSynchronized `elem` af || null mCode)
                        -- || AMVarargs `elem` af || null mCode) -- disabled
                        -- VarArg check, see GSoC Phase 2 goals
                        -- These identifiers enable immediate disqualification
                        -- or if Code is empty implying an abstract method
            then Right (loadedThingsStatus, fDB, M.insert mID UnanalyzableMethod mDB,
                    n_fDB, M.insert mID UnanalyzableNullMethod n_mDB)
            else if isInitialStrongImpure mName
                     then Right ( loadedThingsStatus
                          , fDB
                          , M.insert mID StrongImpure mDB
                          , n_fDB
                          , M.insert mID NullableMethod n_mDB)
                     else if nonEmpty nA
                              then --seriousLogger ("\n From analyseMethod: \n" ++ show thing ++ " ?? " ++ show nA) $ 
                                    if getAbortOnAbsence == Abort
                                    then Left nA
                                    else Right  ( loadedThingsStatus
                                                , fDB
                                                , M.insert mID UnanalyzableMethod mDB
                                                , n_fDB
                                                , M.insert mID UnanalyzableNullMethod n_mDB 
                                                )
                                    
                              else let result = 
                                            feedAll
                                               cpp
                                               loadedThings
                                               classes
                                               loadedThingsStatus
                                               fDB
                                               mDB
                                               n_fDB
                                               n_mDB
                                               nAl
                                   in case result of
                                        Left dnf    -> Left dnf
                                        Right (loadedThingsStatus', fDB', mDB', n_fDB', n_mDB') ->
                                                                                -- here you call feedAll with cmap
                                        -- after this all deps of this method is not in loadedThingsStatus, as they have all been analyzed
                                        -- recursively. So we can now safely analyse this method itself
                                        -- But it may happen that this method itself got into a loop and was analyzed already
                                            if M.member thing loadedThingsStatus'
                                            then 
                                                let !desidx =
                                                        descriptorIndices2 $
                                                        mDes
                                                    !initialHeap =
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
                                                    replaceRefWithNull
                                                        SBasicLong =
                                                            NSNonNullableLong
                                                    replaceRefWithNull _ =
                                                            NSNonNullable
                                                    -- others can be fresh objects
                                                    -- or basic which are
                                                    -- nonnullable

                                                    !lh_null =
                                                        M.map
                                                            replaceRefWithNull
                                                            lh'

                                                    lhp = [lh']
                                                    lhp_null = [lh_null]
                                                    initialState =
                                                        AnalysisBundle
                                                            cpool
                                                            mDB'
                                                            fDB'
                                                            mCFG
                                                            lhp
                                                            [[]]
                                                            [theStart]
                                                            [[]]
                                                            []
                                                    (finalState, verdict) =
                                                        resultant
                                                            getAnalysis
                                                            initialState


                                                    initialState_null =
                                                        NullAnalysisBundle
                                                            mID
                                                            cpool
                                                            n_mDB'
                                                            n_fDB'
                                                            mCFG
                                                            lhp_null
                                                            [[]]
                                                            [theStart]
                                                    (finalState_null, verdict_null) =
                                                        resultant
                                                            getAnalysis_null
                                                            initialState_null


                                                    fDB'' = fieldDB finalState
                                                    mDB'' = methodDB finalState
                                                    n_fDB'' = n_fieldDB finalState_null
                                                    n_mDB'' = n_methodDB finalState_null
                                                    
                                                    dnf = depsNotFound finalState

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

                                                 in if not (null dnf) 
                                                    then Left dnf
                                                    else Right $
                                                        ( loadedThingsStatus' -- can safely leave this like this without
                                                                          -- deleting this method as the caller
                                                                          -- analysisDriver deletes it anyway.
                                                        , fDB''
                                                        , M.insert mID fin mDB''
                                                        , n_fDB''
                                                        , M.insert mID verdict_null n_mDB'' )
                                            else Right $ 
                                                    ( loadedThingsStatus'
                                                    , fDB'
                                                    , mDB'
                                                    , n_fDB'
                                                    , n_mDB' 
                                                    )

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

data StackObject
    = SBasic
    | SBasicLong            -- Category 2, needed for analyseAtom
    | SReference Int
    | SReferenceFresh
    | SObjArrayReference
    deriving (Eq, Show)

data NStackObject
    = NSNullable
    | NSNonNullable
    | NSNonNullableLong     -- Category 2, needed for analyseAtom_null
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
    { cpool :: V.Vector ConstantInfo
    , methodDB :: MethodDB
    , fieldDB :: FieldDB
    , cfg :: CFG
    , lHeaps :: LocalHeaps
    , stacks :: Stacks
    , cpos :: [Int]
    , mutDB :: [MutLocs]
    , depsNotFound :: DepsNotFound
    } deriving (Show)

data NullAnalysisBundle = NullAnalysisBundle
    { n_methodName :: MethodID
    , n_cpool :: V.Vector ConstantInfo
    , n_methodDB :: MethodNullabilityDB
    , n_fieldDB :: FieldNullabilityDB
    , n_cfg :: CFG
    , n_lHeaps :: NLocalHeaps
    , n_stacks :: NStacks
    , n_cpos :: [Int]
    } deriving (Show)

type AnalysisM = FX AnalysisBundle MethodType -- the analysis monad
type NAnalysisM = FX NullAnalysisBundle MethodNullabilityType

getCpool :: AnalysisM (V.Vector ConstantInfo)
getCpool = pure cpool <*> getS

getCpool_null :: NAnalysisM (V.Vector ConstantInfo)
getCpool_null = pure n_cpool <*> getS

getMDB :: AnalysisM MethodDB
getMDB = pure methodDB <*> getS

getMDB_null :: NAnalysisM MethodNullabilityDB
getMDB_null = pure n_methodDB <*> getS

getFDB :: AnalysisM FieldDB
getFDB = pure fieldDB <*> getS

getFDB_null :: NAnalysisM FieldNullabilityDB
getFDB_null = pure n_fieldDB <*> getS

getCfg :: AnalysisM CFG
getCfg = pure cfg <*> getS

getCfg_null :: NAnalysisM CFG
getCfg_null = pure n_cfg <*> getS

getStacks :: AnalysisM Stacks
getStacks = pure stacks <*> getS

getStacks_null :: NAnalysisM NStacks
getStacks_null = pure n_stacks <*> getS

getLocalHeaps :: AnalysisM LocalHeaps
getLocalHeaps = pure lHeaps <*> getS

getLocalHeaps_null :: NAnalysisM NLocalHeaps
getLocalHeaps_null = pure n_lHeaps <*> getS

getCPos :: AnalysisM [Int]
getCPos = pure cpos <*> getS

getCPos_null :: NAnalysisM [Int]
getCPos_null = pure n_cpos <*> getS

setCPos :: [Int] -> AnalysisM ()
setCPos z = do
    {--
    AnalysisBundle a b c d e f _ g <- getS
    putS $ AnalysisBundle a b c d e f z g
    --}
    curAB <- getS
    putS $ curAB {
        cpos = z
    }

setCPos_null :: [Int] -> NAnalysisM ()
setCPos_null z = do
    curNAB <- getS
    putS $ curNAB {
        n_cpos = z
    }

setStacks :: Stacks -> AnalysisM ()
setStacks stks = do
    {--
    AnalysisBundle a b c d e _ f g <- getS
    putS $ AnalysisBundle a b c d e stks f g
    --}
    curAB <- getS
    putS $ curAB {
        stacks = stks
    }

setStacks_null :: NStacks -> NAnalysisM ()
setStacks_null stks = do
    curNAB <- getS
    putS $ curNAB {
        n_stacks = stks
    }

setLocalHeaps :: LocalHeaps -> AnalysisM ()
setLocalHeaps lheaps = do
    {--
    AnalysisBundle a b c d _ f g h <- getS
    putS $ AnalysisBundle a b c d lheaps f g h
    --}
    curAB <- getS
    putS $ curAB {
        lHeaps = lheaps
    }

setLocalHeaps_null :: NLocalHeaps -> NAnalysisM ()
setLocalHeaps_null lheaps = do
    curNAB <- getS
    putS $ curNAB {
        n_lHeaps = lheaps
    }

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs p v = take p xs ++ [v] ++ drop (p + 1) xs

-- remove anything but add short basic things
stackOperate :: Stack -> (Int, Int) -> Stack
stackOperate stk (inp, outp) =
    if length stk < inp
        then error "stackOperate : Stack is smaller than number of inputs required"
        else replicate outp SBasic ++ drop inp stk

stackOperate_null :: NStack -> (Int, Int) -> NStack
stackOperate_null stk (inp, outp) =
    if length stk < inp
        then error "stackOperate_null : Stack is smaller than number of inputs required"
        else replicate outp NSNonNullable ++ drop inp stk

stackOperateL_null :: NStack -> (Int, Int) -> NStack
stackOperateL_null stk (inp, outp) =
    if length stk < inp
        then error "stackOperateL_null : Stack is smaller than number of inputs required"
        else replicate outp NSNonNullableLong ++ drop inp stk


-- remove anything but adds long things
stackOperateL :: Stack -> (Int, Int) -> Stack
stackOperateL stk (inp, outp) =
    if length stk < inp
        then error "Stack is smaller than number of inputs required"
        else replicate outp SBasicLong ++ drop inp stk

setStackPos :: Int -> Stack -> AnalysisM ()
setStackPos pos stk = do
    {--
    AnalysisBundle a b c d e f g h <- getS
    putS $ AnalysisBundle a b c d e (replaceElem f pos stk) g h
    --}

    curAB <- getS
    let f = stacks curAB
    putS $ curAB {
        stacks = replaceElem f pos stk
    }

setStackPos_null :: Int -> NStack -> NAnalysisM ()
setStackPos_null pos stk = do
    curNAB <- getS
    let f = n_stacks curNAB
    putS $ curNAB {
        n_stacks = replaceElem f pos stk
    }

setLocalHeapPos :: Int -> LocalHeap -> AnalysisM ()
setLocalHeapPos pos lheap = do
    {--
    AnalysisBundle a b c d e f g h <- getS
    putS $ AnalysisBundle a b c d (replaceElem e pos lheap) f g h
    --}

    curAB <- getS
    let e = lHeaps curAB
    putS $ curAB {
        lHeaps = replaceElem e pos lheap
    }

setLocalHeapPos_null :: Int -> NLocalHeap -> NAnalysisM ()
setLocalHeapPos_null pos lheap = do
    curNAB <- getS
    let e = n_lHeaps curNAB
    putS $ curNAB {
            n_lHeaps = replaceElem e pos lheap
    }

getLocalHeapElem :: LocalHeap -> Int -> StackObject
getLocalHeapElem loc pos = loc ! pos -- error if does not exist

getLocalHeapElem_null :: NLocalHeap -> Int -> NStackObject
getLocalHeapElem_null loc pos = loc ! pos

localHeapOperate :: LocalHeap -> Int -> StackObject -> LocalHeap
localHeapOperate loc pos obj = M.insert pos obj loc

localHeapOperate_null :: NLocalHeap -> Int -> NStackObject -> NLocalHeap
localHeapOperate_null loc pos obj = M.insert pos obj loc

getMuts :: AnalysisM [MutLocs]
getMuts = pure mutDB <*> getS

setMuts :: [MutLocs] -> AnalysisM ()
setMuts ms = do
    {--
    AnalysisBundle a b c d e f g _ <- getS
    putS $ AnalysisBundle a b c d e f g ms
    --}
    curAB <- getS
    putS $ curAB {
        mutDB = ms
    }

-- add a mutation location to a specific instance
addMut :: Int -> Int -> AnalysisM ()
addMut pos v = do
    mutl <- getMuts
    when (length mutl <= pos) $ debugLoggerM "mutl problem!"
    setMuts $ replaceElem mutl pos (v : (mutl !! pos))

getMethodName_null :: NAnalysisM MethodID
getMethodName_null = pure n_methodName <*> getS

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

consumeCode_null :: NAnalysisM [CodeAtom]
consumeCode_null = do
    g <- getCfg_null
    p <- getCPos_null
    stk <- getStacks_null
    loc <- getLocalHeaps_null
    let q =
            concatMap
                (\(l, s, c) ->
                     zip3 (repeat l) (repeat s) (suc g c)) $
            zip3 loc stk p
    let np = map thirdof3 q
    setCPos_null np
    setLocalHeaps_null $ map firstof3 q
    setStacks_null $ map secondof3 q
    return $ map (nodecode . fromJust . lab g) $ np


getAnalysis :: AnalysisM MethodType
getAnalysis = do
    cas <- consumeCode
    pos <- getCPos
    whenExit (all (== theEnd) pos) Pure -- all of them are at the end
    stks <- getStacks
    locs <- getLocalHeaps
    when (not (length cas == length locs && length locs == length stks)) $
        error "lengths of cas, locs, stks not all equal!"
    results <-
        forM [0 .. length cas - 1] $ \j ->
            exceptify $ analyseAtom j (cas !! j) (locs !! j) (stks !! j)
    mutl <- getMuts

    whenExit (length results > tHRESHOLD) UnanalyzableMethod -- too many branches
    whenExit (UnanalyzableMethod `elem` results) UnanalyzableMethod
    whenExit (StrongImpure `elem` results) StrongImpure
    whenExit (Impure `elem` results) Impure

    getAnalysis


getAnalysis_null :: NAnalysisM MethodNullabilityType
getAnalysis_null = do
    cas <- consumeCode_null
    pos <- getCPos_null
    whenExit (all (== theEnd) pos) NonNullableMethod
    stks <- getStacks_null
    locs <- getLocalHeaps_null

    when (not (length cas == length locs && length locs == length stks)) $
        error "lengths of cas, locs, stks not all equal!_null"

    results <-
        forM [0 .. length cas - 1] $ \j ->
            exceptify $ analyseAtom_null j (cas !! j) (locs !! j) (stks !! j)

    whenExit (length results > tHRESHOLD) UnanalyzableNullMethod
    whenExit (UnanalyzableNullMethod `elem` results) UnanalyzableNullMethod
    whenExit (NullableMethod `elem` results) NullableMethod

    getAnalysis_null

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

getIdxOp :: BL.ByteString -> Int
getIdxOp (BL.uncons -> Just (op, rest)) =
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
analyseAtom j (pos, BL.uncons -> Nothing) loc stk = return Pure
                --traceM $ "Code : " ++ show pos ++ " : " ++ show ca
analyseAtom j (pos, ca@(BL.uncons -> Just (op, rest))) loc stk
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
    when
        ((op == getStatic || op == putStatic) &&
         (isNothing $ isFinalStaticFieldWord8 rest cpool fDB)) $ do
            exitWith UnanalyzableMethod
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
    when (op == getFieldOp) $ exitWith Impure
        {--
        let ftype = getSTypeOfFieldWord8 rest cpool
            topelem = unsafeHead "getFieldOp" stk
            rstk = tail stk
        if | ftype == OFBasic -> setStackPos j (SBasic : rstk)
           | ftype == OFBasicLong -> setStackPos j (SBasicLong : rstk)
           | otherwise -> setStackPos j (topelem : rstk)
        --}
    -- NOTE (2nd July 2018) : The above code has been commented out in response
    -- to the proposal by my GSoC mentors that getField accesses are not pure in
    -- general, because other methods may modify them. As such only final static
    -- field accesses should be allowed. But final static accesses are always
    -- getStatic accesses, so getField is always Impure

    -- SReference Int then this is also SReference Int
    -- If SReferenceFresh then this is also fresh reference

    when (op == putFieldOp) $ do
        let ftype = getSTypeOfFieldWord8 rest cpool
            !obj = head (tail stk) -- item 2
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
            then do
                    addMut j $ fromJust ty -- adding mutation to array
                    setStackPos j rest
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
            then do
                exitWith UnanalyzableMethod
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
            then do
                exitWith UnanalyzableMethod
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


basicReturns, basicLoads, basicLoadsL, aLoads, mathOps21, mathOps11 :: [Word8]
bitwiseInt, bitwiseLong, convert11, convert12, convert22, convert21 :: [Word8]
ifConsume1, ifConsume2, compareConsume2 :: [Word8]

basicReturns = [172..175] ++ [177] -- returns basic things
basicLoads = [2..8] ++ [11..13] ++ [16..17] ++ [21, 23] ++ [26..29] ++
             [34..37] ++
             [46, 48, 51, 52, 53]
                                            -- arrays are considered nonnull
                                            -- if some loading is being done
                                            -- from them


basicLoadsL = ([2..17] ++ [21..24] ++ [26..41] ++ [46..49] ++ [51..53])
                \\ basicLoads

aLoads = [25, 42, 43, 44, 45]

mathOps21 = [96..115]
mathOps11 = [116..119]

bitwiseInt = [120, 122, 124, 126, 128, 130]
bitwiseLong = [121, 123, 125, 127, 129, 131]

-- convert cat i to cat j is convertij
convert12 = [133, 135, 140, 141]
convert11 = [134, 139,            145, 146, 147]
convert21 = [136, 137, 142, 144]
convert22 = [138, 143]

ifConsume1 = [153 .. 158] ++ [170, 171, 198, 199]
                                    -- tableswitch and lookupswitch also work
                                    -- like ifs, by consuming one value
                                    -- same for ifnonnull and ifnull
ifConsume2 = [159 .. 166]

compareConsume2 = [148 .. 152]

arrayStoresConsume3 = [79 .. 86]

analyseAtom_null :: Int -> CodeAtom -> NLocalHeap -> NStack -> NAnalysisM MethodNullabilityType
analyseAtom_null j (pos, BL.uncons -> Nothing) loc stk = return NonNullableMethod
analyseAtom_null j (pos, ca@(BL.uncons -> Just (op, rest))) loc stk = do
    whenExit (op == monitorEnterOp ||
              op == monitorExitOp  ||
              op == invokeInterfaceOp ||
              op == invokeDynamicOp)
        UnanalyzableNullMethod

    whenExit (op == 0) NonNullableMethod -- nop => this thread is not nullable

    whenExit (op == wideOp) UnanalyzableNullMethod
    whenExit (op == aThrowOp) UnanalyzableNullMethod

    cpool <- getCpool_null
    n_fDB <- getFDB_null
    n_mDB <- getMDB_null

    when (op == aconstNullOp) $     -- this is the main step :)
        setStackPos_null j (NSNullable : stk)

    whenExit (op == areturnOp) $
        let topelem = unsafeHead "Return from analyseAtom_null" stk
        in  if topelem == NSNullable
            then NullableMethod
            else NonNullableMethod -- dont worry about other threads
                                   -- the current thread says this.
                                   -- that merging of data is done
                                   -- by getAnalysis_null

    whenExit (op `elem` basicReturns) NonNullableMethod
        -- op codes that return basic stuff is always nonnullable
        -- void is nonnullable in particular

    when (op == newOp || op ==newArrayOp
            || op == anewArrayOp || op == multianewArrayOp) $
        setStackPos_null j (NSNonNullable : stk)    -- new stuff is nonnull

    when (op == checkCastOp) $ return ()

    when (op == 167 || op == 200) $ return () -- goto/goto_w op, no change

    when (op == 190 || op == 193) $ -- array length and instanceof
        setStackPos_null j (NSNonNullable : (tail stk))

    -- ======================= Misc stuff finished ================ --
    -- For loads we don't look at local heaps assuming they are correct.

    when (op `elem` basicLoads) $
        setStackPos_null j (NSNonNullable : stk)
                                             -- basic loads only add
                                             -- non nullable stuff
                                             -- to the stack
    when (op `elem` basicLoadsL) $
        setStackPos_null j (NSNonNullableLong : stk)
                                            -- Category 2 stuff but same
                                            -- as above.
    when (op `elem` aLoads) $ do -- object loads must look at local heap
        let idx = getIdxOp ca
            elm = getLocalHeapElem_null loc idx
        setStackPos_null j (elm : stk)

    when (op == aaloadOp) $ do  -- assuming the pointed to array is nonnull
                                    -- but there is no guarantee that the object
                                    -- loaded is not null
        setStackPos_null j (NSNullable : stk)

    when (op == ldcOp || op == ldc_wOp) $ do
        let ftype = getSTypeOfConstantWord8 rest cpool
        if | ftype == OFBasic   ->
                    setStackPos_null j (NSNonNullable : stk)
           | ftype == OFBasicLong ->
                    setStackPos_null j (NSNonNullableLong : stk)
           | ftype == OFReference ->
                    setStackPos_null j (NSNonNullable : stk)
                    -- this is nonnullable because constant references
                    -- are always nonnull like strings etc.
           | otherwise  ->
                    exitWith UnanalyzableNullMethod

    when (op == 20) $   -- ldc2_w ; always loads category 2 types
        setStackPos_null j (NSNonNullableLong : stk)

    -- =============== Loads finished =================== --
    -- For stores we have to look at local heaps
    -- Take the thing that is on the stack and put it in the
    -- respective location in the local heap.
    -- NOTE : There is no special checking involved.
    when (op `elem` stores) $ do -- this case just handles the non-array stores
        let idx          = getIdxOp ca
            (elm : rest) = stk --debugLogger ("Stack size is " ++ show (length stk))
                            --stk

        setLocalHeapPos_null j $ localHeapOperate_null loc idx elm
        setStackPos_null j rest -- remove element from stack

    -- deals with iastore, aastore etc. Each of them affects
    -- a arrayref and commits nothing to local heap, so just drop
    -- 3 elements from the stack
    when (op `elem` arrayStoresConsume3) $
        setStackPos_null j (drop 3 stk)

    -- =============== Stores finished =================== --

    when (op == popOp) $ setStackPos_null j $ tail stk
    when (op == pop2Op) $ setStackPos_null j $ tail $ tail stk
    when (op == dupOp) $ do
        let ~(e1:rest) = stk
        setStackPos_null j $ [e1, e1] ++ rest
    when (op == dup_x1Op) $ do
        let ~(e1:(e2:rest)) = stk
        setStackPos_null j $ [e1, e2, e1] ++ rest
    when (op == dup_x2Op) $ do
        let ~(e1:(e2:rest)) = stk
        if isCat2_null e2
            then setStackPos_null j $ [e1, e2, e1] ++ rest
            else let (e3:rest') = rest
                  in setStackPos_null j $ [e1, e2, e3, e1] ++ rest'
    when (op == dup2Op) $ do
        let ~(e1:rest) = stk
        if isCat2_null e1
            then setStackPos_null j $ [e1, e1] ++ rest
            else let ~(e2:rest) = stk
                  in setStackPos_null j $ [e1, e2, e1, e2] ++ rest
    when (op == dup2_x1Op) $ do
        let ~(e1:(e2:rest)) = stk
        if isCat2_null e1
            then setStackPos_null j $ [e1, e2, e1] ++ rest
            else let ~(e3:rest') = stk
                  in setStackPos_null j $ [e1, e2, e3, e1, e2] ++ rest'
    when (op == dup2_x2Op) $ do
        let ~(e1:(e2:(e3:rest))) = stk
        if isCat2_null e1
            then setStackPos_null j $ [e1, e2, e3, e1] ++ rest
            else let ~(e4:rest') = rest
                  in setStackPos_null j $ [e1, e2, e3, e4, e1, e2] ++ rest'
    when (op == swapOp) $ do
        let ~(e1:(e2:rest)) = stk
        setStackPos_null j $ [e2, e1] ++ rest

    -- =============== dups, swap etc end ================= --

    when (op `elem` mathOps21) $ do -- remove two and insert one of the same
                                    -- type. So just remove one.
        setStackPos_null j $ tail stk

    when (op `elem` mathOps11) $ return () -- remove 1 and insert 1 of same type
                                           -- so equivalent to no-op for this
                                           -- analysis

    when (op `elem` (bitwiseInt ++ bitwiseLong)) $ do
        let ~(a : (b : rest)) = stk
        setStackPos_null j (b : rest)       -- for the case of lshl, ishl
                                            -- top value is the shift which is
                                            -- int. So in all cases, the second
                                            -- element is the one whose type is
                                            -- the type of the answer.

    when (op == 132) $ return ()    -- iinc, no change.

    when (op `elem` (convert11 ++ convert22)) $ return ()

    when (op `elem` convert12) $
        setStackPos_null j (NSNonNullableLong : (tail stk))

    when (op `elem` convert21) $
        setStackPos_null j (NSNonNullable : (tail stk))

    when (op `elem` ifConsume1) $
        setStackPos_null j (tail stk)

    when (op `elem` ifConsume2) $
        setStackPos_null j (drop 2 stk)

    when (op `elem` compareConsume2) $
        setStackPos_null j (NSNonNullable : (drop 2 stk))

    -- ================ math type ops done =============== --

    when (op == getStatic) $ do
        let fID     = getFieldIDWord8 rest cpool
            ty      = n_fDB !? fID
            ftype   = getSTypeOfFieldWord8 rest cpool
        if isNothing ty
           then exitWith UnanalyzableNullMethod
                    -- whenExit can return from the deepest
                    -- when-s as well, for why this happens
                    -- look at the implementation for MonadFX
           else if fromJust ty == NullableField
                then setStackPos_null j (NSNullable : stk)
                else if ftype == OFBasicLong
                     then setStackPos_null j (NSNonNullableLong : stk)
                     else setStackPos_null j (NSNonNullable : stk)
                        -- this last else is for either basic non long types
                        -- like int or nonnull references

    -- getField and getStatic are the same other than the fact
    -- that getField will take an object ref, so the top elem
    -- from the stack has to be popped

    when (op == getFieldOp) $ do
        let fID     = getFieldIDWord8 rest cpool
            ty      = n_fDB !? fID
            ftype   = getSTypeOfFieldWord8 rest cpool
            stk'    = tail stk
        if isNothing ty
           then exitWith UnanalyzableNullMethod
                    -- whenExit can return from the deepest
                    -- when-s as well, for why this happens
                    -- look at the implementation for MonadFX
           else if fromJust ty == NullableField
                then setStackPos_null j (NSNullable : stk')
                else if ftype == OFBasicLong
                     then setStackPos_null j (NSNonNullableLong : stk')
                     else setStackPos_null j (NSNonNullable : stk')


    -- putStatic and putField are also treated similarly now
    -- but later putStatic must be modified to accommodate
    -- final static initialization in <clinit>
    when (op == putStatic) $ setStackPos_null j (tail stk)
    when (op == putFieldOp)  $ setStackPos_null j (drop 2 stk)

    when (op == invokeVirtualOp || op == invokeSpecialOp) $ do
        let mID = getMethodName cpool (fromIntegral $ toIndex rest)
            nar = length $ descriptorIndices $ snd mID
            ~(obj:argsrem) = stk
            args = take nar argsrem
            oth = drop nar argsrem
            mty = n_mDB !? mID
        if isNothing mty
            then exitWith UnanalyzableNullMethod
            else let ret = getReturnType mID
                 in  if | ret == RTBasic    ->
                                    setStackPos_null j (NSNonNullable : oth)
                        | ret == RTBasicLong ->
                                    setStackPos_null j (NSNonNullableLong : oth)
                        | ret == RTVoid ->
                                    setStackPos_null j oth
                        | otherwise -> if fromJust mty == NullableMethod
                                       then setStackPos_null j
                                                (NSNullable : oth)
                                       else setStackPos_null j
                                                (NSNonNullable : oth)
                                -- inserting references which are either
                                -- nullable or nonnullable

    -- invokeStatic is almost the same as invokeSpecial and Virtual
    -- except that there is no object being passed implicitly.
    when (op == invokeStaticOp) $ do
        let mID = getMethodName cpool (fromIntegral $ toIndex rest)
            nar = length $ descriptorIndices $ snd mID
            argsrem = stk
            args = take nar argsrem
            oth = drop nar argsrem
            mty = n_mDB !? mID
        if isNothing mty
            then exitWith UnanalyzableNullMethod
            else let ret = getReturnType mID
                 in  if | ret == RTBasic    ->
                                    setStackPos_null j (NSNonNullable : oth)
                        | ret == RTBasicLong ->
                                    setStackPos_null j (NSNonNullableLong : oth)
                        | ret == RTVoid ->
                                    setStackPos_null j oth
                        | otherwise -> if fromJust mty == NullableMethod
                                       then setStackPos_null j
                                                (NSNullable : oth)
                                       else setStackPos_null j
                                                (NSNonNullable : oth)
                                -- inserting references which are either
                                -- nullable or nonnullable

    -- final returns
    return NonNullableMethod



getFieldIDWord8 :: BL.ByteString -> V.Vector ConstantInfo -> FieldID
getFieldIDWord8 rest cpool
    =   let idx = toIndex rest
        in getFieldName cpool (fromIntegral idx)

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
    let ret = T.reverse $ T.takeWhile (/= ')') $ T.reverse des
        fc = T.head ret
     in if | fc `elem` basicList -> RTBasic
           | fc `elem` longList -> RTBasicLong
           | fc `elem` refList -> RTReference
           | fc `elem` voidElem -> RTVoid
           | otherwise ->
               error $
               "Unknown return type for method with descriptor : " ++ show des

isCat2_null :: NStackObject -> Bool
isCat2_null NSNonNullableLong = True
isCat2_null _ = False

isCat2 :: StackObject -> Bool
isCat2 SBasicLong = True
isCat2 _ = False

unParamRef :: StackObject -> Int
unParamRef (SReference x) = x
unParamRef _ = error "unParamRef called on non SReference type"

isParamRef :: StackObject -> Maybe Int
isParamRef (SReference x) = Just x
isParamRef _ = Nothing

getSTypeOfConstantWord8 :: BL.ByteString -> V.Vector ConstantInfo -> ObjectField
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
       BL.ByteString -> V.Vector ConstantInfo -> FieldDB -> Bool
isFinalStaticFieldWord8MaybeStripped a b c =
    (\m ->
         if isJust m
             then fromJust m
             else False) $
    isFinalStaticFieldWord8 a b c

isFinalStaticFieldWord8 :: BL.ByteString -> V.Vector ConstantInfo -> FieldDB -> Maybe Bool
isFinalStaticFieldWord8 rest cpool fdb =
    let idx = toIndex rest
        fID = getFieldName cpool (fromIntegral idx)
        fty = fdb !? fID
     in if isJust fty
            then Just $ fromJust fty == FinalStatic
            else Nothing

getSTypeOfFieldWord8 :: BL.ByteString -> V.Vector ConstantInfo -> ObjectField
getSTypeOfFieldWord8 rest cpool =
    getSTypeOfField (fromIntegral $ toIndex rest) cpool

getSTypeOfField :: Int -> V.Vector ConstantInfo -> ObjectField
getSTypeOfField idx cpool =
    let des = T.head $ snd $ getFieldName cpool idx
     in if | des `elem` basicList -> OFBasic
           | des `elem` longList -> OFBasicLong
           | des `elem` refList -> OFReference
           | otherwise -> error $ "Invalid Field Type of character " ++ show des

descriptorIndices2 :: T.Text -> [(Int, StackObject)]
descriptorIndices2 descriptor = recursiveCalc desc2 1
  where
    desc2 = T.takeWhile (')' /=) $ T.drop 1 descriptor -- Convert (xxx)yyy -> xxx
    recursiveCalc :: T.Text -> Int -> [(Int, StackObject)]
    recursiveCalc ("") x = []
    recursiveCalc des x =
        if c `elem` (basicList ++ longList)
            then if c `elem` longList
                     then (x, SBasicLong) : recursiveCalc left (x + 2)
                     else (x, SBasic) : recursiveCalc left (x + 1)
            else if c == 'L'
                     then (x, SReference x) :
                          recursiveCalc
                              (T.drop 1 $ T.dropWhile (';' /=) left)
                              (x + 1)
                     else if c == '['
                              then let arrTypeAnd = T.dropWhile ('[' ==) left
                                       arrType =
                                           T.head arrTypeAnd
                                    in if arrType == 'L'
                                           then (x, SReference x) :
                                                recursiveCalc
                                                    (T.drop 1 $
                                                     T.dropWhile
                                                         (';' /=)
                                                         arrTypeAnd)
                                                    (x + 1)
                                           else (x, SReference x) :
                                                recursiveCalc
                                                    (T.drop 1 arrTypeAnd)
                                                    (x + 1)
                              else []
        where
            c       = T.head des
            left    = T.tail des
