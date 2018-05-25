{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Etanol.Decompile (
                getFieldName,
                getMethodName,
                DNodeData(..),
                DEdgeData(..),
                DCFG(..),
                DCodeAtom(..),
                primStoreIndexed, primLoadIndexed,
                primLoadInbuilt0, primLoadInbuilt1, primLoadInbuilt2, primLoadInbuilt3,
                primStoreInbuilt0, primStoreInbuilt1, primStoreInbuilt2, primStoreInbuilt3,
                getStatic, putStatic
                ) where

import Data.Binary (Word8, Word16, Word32)

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import ByteCodeParser.Instructions

import Etanol.ControlFlowGraph
import Etanol.Types

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot (showDot, fglToDot)

data DNodeData = DNodeData {
                --dcode :: DCodeAtom
        } deriving (Show, Eq)

data DEdgeData = DEdgeData deriving (Show, Eq)

type DCFG = Gr DNodeData DEdgeData

data DCodeAtom =        NoOp {
                                ucode :: [Word8]
                        } |
                        LoadPrimaryS {
                                ucode :: [Word8],
                                pos   :: Int
                        } |
                        GetStaticS {
                                ucode :: [Word8],
                                fieldname :: String
                        } |
                        PutStaticS {
                                ucode :: [Word8],
                                fieldname :: String
                        } 

primLoadIndexed, primLoadInbuilt0, primLoadInbuilt1, primLoadInbuilt2, primLoadInbuilt3 :: [Word8]
primLoadIndexed  = [21, 22, 23, 24]
primLoadInbuilt0 = [26, 30, 34, 38]
primLoadInbuilt1 = [27, 31, 35, 39]
primLoadInbuilt2 = [28, 32, 36, 40]
primLoadInbuilt3 = [29, 33, 37, 41]

primStoreIndexed, primStoreInbuilt0, primStoreInbuilt1, primStoreInbuilt2, primStoreInbuilt3 :: [Word8]
primStoreIndexed  = [54, 55, 56, 57]
primStoreInbuilt0 = [59, 63, 67, 71]
primStoreInbuilt1 = [60, 64, 68, 72]
primStoreInbuilt2 = [61, 65, 69, 73]
primStoreInbuilt3 = [62, 66, 70, 74]


equivNoOp :: [Word8] -- instructions that have no effect on analysis
equivNoOp = [0] ++ [2..15] ++ [96..131] ++ [133..147] ++ [148..152] 
--         noop   loadconst     math         cast          compare

javaNamify :: String -> String
javaNamify = map (\x -> if x == '/' then '.' else x)

getFieldName  :: [ConstantInfo] -> Int -> FieldID
getFieldName cp pos = let       fld = cp !! pos
                                cn  = string $ info $ cp !@ (nameIndex $ info $ cp !@ (classIndex $ info fld))
                                nt  = info $ cp !@ (nameAndTypeIndex $ info fld)
                                nm  = string $ info $ cp !@ (nameIndex nt)
                                des = string $ info $ cp !@ (descriptorIndex nt)
                                
                  in (javaNamify $ adjoinClassName cn nm, des)



getMethodName :: [ConstantInfo] -> Int -> MethodID
getMethodName cp pos = let      mthd = cp !! pos
                                cn   = string $ info $ cp !@ (nameIndex $ info $ cp !@ (classIndex $ info mthd))
                                nt   = info $ cp !@ (nameAndTypeIndex $ info mthd)
                                nm   = string $ info $ cp !@ (nameIndex nt)
                                des  = string $ info $ cp !@ (descriptorIndex nt)
                       in (javaNamify $ adjoinClassName cn nm, des)
 

getStatic, putStatic :: Word8
getStatic = 178
putStatic = 179



readPos :: CFG -> Int -> (Int, DCodeAtom)
readPos g x = undefined

decompileEssential :: CFG -> DCFG
decompileEssential = undefined



