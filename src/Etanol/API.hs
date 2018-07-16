{-# LANGUAGE RecordWildCards #-}
module Etanol.API 
    ( explore
    , depsNotFoundByExplore
    , rawClassFileProvider
    , constPoolProvider
    , AnyID (..)
    , FieldID
    , MethodID
    , ClassProvider
    , FieldType (..)
    , MethodType (..)
    , FieldNullabilityType (..)
    , MethodNullabilityType (..)
    , FieldDB 
    , MethodDB
    , FieldNullabilityDB
    , MethodNullabilityDB
    , analysis
    , AnalysisInputType (..)
    , AnalysisOutputType (..)
    ) where

import qualified EtanolTools.Unsafe     as U

import Etanol.Analysis
import Etanol.Decompile
import Etanol.Types
import Etanol.Utils

import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import qualified Data.Map               as M
import qualified Data.Vector            as V
import qualified Data.Set               as S
import Data.Set (difference, union)
import Data.Map ((!?))

import Data.List hiding (union)
import Data.Maybe
import Control.Monad

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader

import Control.Monad.ST
import Data.STRef

type ClassProvider = ClassName -> Maybe BL.ByteString
type RawClassFileProvider = ClassName -> Maybe RawClassFile

data AnalysisInputType 
    = AnalysisInputType 
    { classes :: [ClassName] 
    , classProvider :: ClassProvider
    , sourceFieldDB :: FieldDB
    , sourceMethodDB :: MethodDB
    , sourceFieldNullabilityDB :: FieldNullabilityDB
    , sourceMethodNullabilityDB :: MethodNullabilityDB
    } 

data AnalysisOutputType
    = AnalysisOutputType
    { fieldPurity :: FieldID -> Maybe FieldType
    , methodPurity :: MethodID -> Maybe MethodType
    , fieldNullability :: FieldID -> Maybe FieldNullabilityType
    , methodNullability :: MethodID -> Maybe MethodNullabilityType
    , fieldsAnalyzed :: [FieldID]
    , methodsAnalyzed :: [MethodID]
    } 

rawClassFileProvider :: ClassProvider -> RawClassFileProvider
rawClassFileProvider cfp = fmap readRawByteString . cfp

constPoolProvider :: ClassProvider -> CPoolProvider
constPoolProvider cp = rawClassFileProvider cp >=> Just . constantPool

explore :: CPoolProvider -> [ClassName] -> [ClassName] --dependencies
explore cpp cns = runST $ do
    seen <- newSTRef (S.fromList cns)
    vis  <- newSTRef S.empty

    exploreVisit cpp seen vis

    visited <- readSTRef vis

    return $ S.toList visited

    where   

        classDeps' :: Maybe (V.Vector ConstantInfo) -> [ClassName]
        classDeps' Nothing = []
        classDeps' (Just cpool) = classDeps cpool 

        exploreVisit cpp seen vis = do
            isNull <- S.null <$> readSTRef seen
            when (not isNull) $ do
                seenS <- readSTRef seen
                visS <- readSTRef vis

                let top = S.elemAt 0 seenS
                    cpool = cpp top
                    deps = S.fromList $ classDeps' (cpp top)
                    deps' = deps `difference` visS
                    seenS' = (S.deleteAt 0 seenS) `union` deps'
                    visS' = S.insert top visS
                
                writeSTRef seen seenS'
                writeSTRef vis  visS'

                exploreVisit cpp seen vis

depsNotFoundByExplore :: CPoolProvider -> [ClassName] -> [ClassName]
depsNotFoundByExplore cpp cns = filter (isNothing . cpp) cns

wrap :: (Show a) => (a -> Maybe b) -> (a -> Maybe b)
wrap cp x = 
    let r = cp x 
    in case r of 
        Nothing     -> U.seriousLogger (" ## " ++ show x) r
        _           -> r

analysis :: AnalysisInputType -> Either [ClassName] AnalysisOutputType
analysis AnalysisInputType{..} = 
    let poolProv = constPoolProvider classProvider
        comp0 = explore poolProv classes
        notf = depsNotFoundByExplore poolProv comp0
        comp = comp0 \\ notf

        readAll = M.fromList $ 
                    map (\cn ->
                            (cn, readRawByteString $ fromJust $ classProvider cn))
                        comp
        
        classProvider' = (readAll !?)
        poolProv'      = fmap constantPool . classProvider'
        f = fromJust . wrap classProvider'
        mthds = concatMap (getMethods . f) comp
        flds  = concatMap (getFields . f)  comp
        mids = map toMethodID mthds
        fids = map toFieldID flds
        loadedThings = M.fromList $!
                map (\(i, d) -> (EFieldID i, EFieldData d)) (zip fids flds) ++
                map (\(i, d) -> (EMethodID i, EMethodData d)) (zip mids mthds)
        loadedStatus = M.fromList $!
                map (\i -> (EFieldID i, NotAnalyzed)) fids ++
                map (\i -> (EMethodID i, NotAnalyzed)) mids
        
        result                      = analyseAll 
                                            poolProv'
                                            (S.fromList comp)
                                            loadedThings
                                            loadedStatus
                                            sourceFieldDB
                                            sourceMethodDB
                                            sourceFieldNullabilityDB
                                            sourceMethodNullabilityDB
        
    in if (U.getAbortOnAbsence == U.Abort) && (not $ null notf)
        then Left notf
        else case result of
                Left dnf    -> Left $ uniqueClasses $ map anyIDToClassName dnf
                Right (_, fdb, mdb, fdbn, mdbn) -> Right $ AnalysisOutputType 
                                                { fieldPurity = (fdb !?)
                                                , methodPurity = (mdb !?)
                                                , fieldNullability = (fdbn !?)
                                                , methodNullability = (mdbn !?)
                                                , fieldsAnalyzed = fids
                                                , methodsAnalyzed = mids
                                                }

uniqueClasses :: [T.Text] -> [T.Text]
uniqueClasses = map head . group . sort
                                                