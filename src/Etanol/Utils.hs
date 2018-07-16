{-# LANGUAGE TupleSections #-}

module Etanol.Utils 
    ( classDeps
    , resolver
    , genDependencyPools
    , weirdClass
    ) where

import           ByteCodeParser.BasicTypes
import           Etanol.Decompile
import qualified EtanolTools.Unsafe as U

import qualified Data.Text          as T
import           Data.List
import           Control.Monad.ST
import qualified Data.Map           as M
import           Data.Map ((!?))

import           Data.Maybe
import qualified Data.Vector        as V
import           Data.Vector ((!))

import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Tree      as G
import qualified Data.Graph.Inductive.Query.DFS as DFS

toClassName :: T.Text -> ClassName  -- remove trailing stuff
toClassName s = T.reverse $ T.tail $ T.dropWhile (/= '.') $ T.reverse s

weirdClass :: ClassName -> Bool
weirdClass cn = T.head cn `elem` ['[', 'L']

classDeps :: (V.Vector ConstantInfo) -> [ClassName]  -- one step class dependencies
classDeps cpool = filter (not . weirdClass) $
                    uniques $! (map (toClassName . fst . getFieldName cpool) $
                                    filter (\elem -> 
                                            constType 
                                             (cpool ! elem) == CFieldRef)
                                        [0..n]) ++
                               (map (toClassName . fst . getMethodName cpool) $
                                    filter (\elem ->
                                            constType
                                             (cpool ! elem) == CMethodRef)
                                        [0..n]) ++ 
                               (map (toClassName . fst . getMethodName cpool) $
                                    filter (\elem ->
                                            constType
                                             (cpool ! elem) == CInterfaceMethodRef)
                                        [0..n])  
                where n =  V.length cpool - 1
        

uniques :: [ClassName] -> [ClassName]
uniques = map head . group . sort

getFastOperators :: (Ord a, Eq a, Show a) => 
                    [a] -> 
                    (Int, Int -> Maybe a, a -> Maybe Int)

getFastOperators xs = let forwardMap = M.fromList $ zip [1..] xs  -- these start                                  
                          reverseMap = M.fromList $ zip xs [1..]  -- from 1.
                      in  (length xs, (forwardMap !?), (\el ->
                                                             (reverseMap !? el)))


genDependencyPools :: (ClassName -> Maybe (V.Vector ConstantInfo)) -- or CPoolProvider
                    -> [ClassName]
                    -> [[ClassName]]
genDependencyPools constPoolOfClass classNames =
    let (n, atIndex, indexOf) = getFastOperators classNames
        edges      = concatMap (\name -> 
                                    let pos = fromJust $ indexOf name
                                    in 
                                       map (\x -> (pos, fromJust $ indexOf x, ())) $
                                        filter (isJust . indexOf) $ 
                                            let cpool = constPoolOfClass name
                                            in  if isNothing cpool
                                                then []
                                                else classDeps $ fromJust cpool)
                     classNames

        nodes      = map (\z -> (z, fromJust $ atIndex z)) [1..n] 
        graph      = mkGraph nodes edges :: G.Gr ClassName ()
        
        scc        = DFS.scc graph  
        scc'       = map (map (fromJust . lab graph)) scc 
    in  U.debugLogger ("Computing the SCC...") $ reverse scc'


resolver :: (ClassName -> Maybe (V.Vector ConstantInfo))
         -> [ClassName]
         -> [ClassName]
resolver = undefined

{--
resolver constPoolOfClass classNames = 
    let allClasses = exhaustive constPoolOfClass classNames
        
        (n, atIndex, indexOf)
                   = getFastOperators allClasses

        edges      = concatMap (\name -> 
                                    let pos = fromJust $ indexOf name
                                    in map (\x -> (pos, fromJust $ indexOf x, ())) $
                                        let cpool = constPoolOfClass name
                                        in  if isNothing cpool
                                             then []
                                             else classDeps $ fromJust cpool)
                        allClasses
        nodes      = map (\z -> (z, atIndex z)) [1..n]

        graph      = mkGraph nodes edges  :: G.Gr ClassName ()

        inorder    = DFS.topsort' graph

    in  U.debugLogger (show classNames ++ " -> " ++ show allClasses) inorder

exhaustive :: (ClassName -> Maybe (V.Vector ConstantInfo))
            -> [ClassName] 
            -> [ClassName]
exhaustive constPoolOfClass classNames = 
    let fullDeps = concatMap (\name -> let cpool = constPoolOfClass name
                                       in  if isNothing cpool
                                           then []     
                                           else (exhaustive constPoolOfClass 
                                                    $ classDeps $ fromJust cpool)
                             )
                    classNames

    in  uniques $ fullDeps ++ classNames
--}
