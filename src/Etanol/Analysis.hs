{-# LANGUAGE DuplicateRecordFields #-}

module Etanol.Analysis (
                getField, putField, invokeVirtual, invokeStatic, invokeSpecial,
                isFinalStaticField, dependencies, AnyID(..), uniques
        ) where

-- update what is being imported

import Data.Binary (Word8, Word16, Word32)
import Data.Maybe 
import Data.Map.Strict ((!?))
import Data.List
import Debug.Trace (trace)

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import ByteCodeParser.Instructions
import Etanol.Types
import Etanol.ControlFlowGraph
import Etanol.Decompile

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

-- Figure out the direct dependencies of a piece of code given the constant pool
dependencies :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
dependencies cpool codes = uniques $ deps cpool codes   

-- Note that this does not need to account for new or anewarray or multianewarray as
-- their constructors are called shortly after these declarations, including them in
-- the dependencies anyway

deps :: [ConstantInfo] -> [CodeAtom] -> [AnyID]
deps cpool [] = []
deps cpool ((idx, op : rest) : restcode)
        | op `elem` [getField, putField, getStatic, putStatic]          = let   idx = toIndex rest
                                                                                fld = trace (show idx) $ getFieldName cpool $ fromIntegral idx
                                                                          in [EFieldID fld] ++ deps cpool restcode
        | op `elem` [invokeSpecial, invokeStatic, invokeVirtual]        = let   idx = toIndex rest
                                                                                mthd = trace (show idx) $ getMethodName cpool $ fromIntegral idx
                                                                          in [EMethodID mthd] ++ deps cpool restcode
        | otherwise                                                     = deps cpool restcode
        
-- Returns true if method cycles to itself during analysis or if method calls a field that is unreachable
-- isUnanalyzable :: [ConstantInfo] -> NamedMethodID -> IO Bool


-- simplePredicates :: [ConstantInfo] -> MethodDB -> FieldDB -> [CodeAtom] -> MethodType
-- simplePredicates cpool methodDB fieldDB ((op : rest) : codes) = undefined
                                                          --
                                                          --
