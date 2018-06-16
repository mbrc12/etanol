{-# LANGUAGE DeriveGeneric, DefaultSignatures, OverloadedStrings,
  ScopedTypeVariables, DuplicateRecordFields #-}

{- Etanol.Types. Includes basic Database functionality for Etanol field and method data.
Also includes helper functions for Etanol.Analysis
-}
module Etanol.Types
  ( Descriptor(..)
  , getClassName
  , FieldType(..)
  , MethodType(..)
  , FieldNullType(..)
  , MethodNullType(..)
  , FieldName
  , MethodName
  , isInit
  , initDB
  , saveFieldDB
  , saveMethodDB
  , getMethodDB
  , getFieldDB
  , FieldDB(..)
  , MethodDB(..)
  , NamedMethodCode(..)
  , NamedField(..)
  , getMethods
  , getFields
  , FieldDescriptor(..)
  , MethodDescriptor(..)
  , FieldID(..)
  , MethodID(..)
  , firstof2
  , secondof2
  , firstof3
  , secondof3
  , thirdof3
  , firstof4
  , secondof4
  , thirdof4
  , fourthof4
  , adjoinClassName
  , toFieldID
  , toMethodID
  ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), (!?))

import qualified Data.ByteString as B
import Debug.Trace (trace)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.IO

import ByteCodeParser.BasicTypes hiding (ClassName)
import ByteCodeParser.Instructions
import ByteCodeParser.Reader

import Etanol.ControlFlowGraph

import Data.Serialize
import qualified Data.Yaml as Y
import GHC.Generics

unsafeHead :: String -> [a] -> a
unsafeHead err xs =
  if null xs
    then (error err)
    else (head xs)

getClassName :: String -> String
getClassName = reverse . tail . dropWhile (/= '.') . reverse

fieldsFile, methodsFile :: FilePath
fieldsFile = "fields.db"

methodsFile = "methods.db"

firstof2 :: (a, b) -> a
firstof2 (a, b) = a

secondof2 :: (a, b) -> b
secondof2 (a, b) = b

firstof3 :: (a, b, c) -> a
firstof3 (a, b, c) = a

secondof3 :: (a, b, c) -> b
secondof3 (a, b, c) = b

thirdof3 :: (a, b, c) -> c
thirdof3 (a, b, c) = c

firstof4 :: (a, b, c, d) -> a
secondof4 :: (a, b, c, d) -> b
thirdof4 :: (a, b, c, d) -> c
fourthof4 :: (a, b, c, d) -> d
firstof4 (a, b, c, d) = a

secondof4 (a, b, c, d) = b

thirdof4 (a, b, c, d) = c

fourthof4 (a, b, c, d) = d

type FieldDescriptor = String

type MethodDescriptor = String

type FieldName = String

type MethodName = String

type ClassName = String

type FieldID = (FieldName, FieldDescriptor)

type MethodID = (MethodName, MethodDescriptor)

data FieldType
  = Normal
  | Basic
  | FinalStatic
  | UnanalyzableField
  deriving (Show, Eq, Ord, Generic)

data FieldNullType
  = NonNullField
  | NullableField
  deriving (Show, Eq, Ord, Generic)

instance Serialize FieldType -- heavylifting done by deriving Generic 

instance Y.FromJSON FieldType

instance Y.ToJSON FieldType

data MethodType
  = Pure
  | Impure
  | Local
  | StrongImpure
  | UnanalyzableMethod
  deriving (Show, Eq, Ord, Generic)

data MethodNullType
  = NonNullMethod
  | NullableMethod
  deriving (Show, Eq, Ord, Generic)

instance Serialize MethodType

instance Y.FromJSON MethodType

instance Y.ToJSON MethodType

type FieldDB = M.Map FieldID FieldType

type MethodDB = M.Map MethodID MethodType

isInit :: FilePath -> IO Bool
isInit configLocation =
  let fieldDBPath = configLocation </> fieldsFile
      methodDBPath = configLocation </> methodsFile
   in do finit <- doesFileExist fieldDBPath
         minit <- doesFileExist methodDBPath
         return $ finit && minit

initDB :: FilePath -> IO ()
initDB configLocation =
  let fieldDBpath = configLocation </> fieldsFile
      methodDBpath = configLocation </> methodsFile
      fieldS = encode (M.empty :: FieldDB)
      methodS = encode (M.empty :: MethodDB)
   in do B.writeFile fieldDBpath fieldS
         B.writeFile methodDBpath methodS

getFieldDB :: FilePath -> IO FieldDB
getFieldDB configLocation =
  let fieldDBPath = configLocation </> fieldsFile
   in do bs <- B.readFile fieldDBPath
         let dec = decode bs
         case dec of
           Right map -> return map
           Left _ -> die "Invalid field database file!"

getMethodDB :: FilePath -> IO MethodDB
getMethodDB configLocation =
  let methodDBPath = configLocation </> methodsFile
   in do bs <- B.readFile methodDBPath
         let dec = decode bs
         case dec of
           Right map -> return map
           Left _ -> die "Invalid method database file!"

saveFieldDB :: FilePath -> FieldDB -> IO ()
saveFieldDB configLocation map =
  let fieldDBPath = configLocation </> fieldsFile
      bs = encode map
   in do B.writeFile fieldDBPath bs

saveMethodDB :: FilePath -> MethodDB -> IO ()
saveMethodDB configLocation map =
  let methodDBPath = configLocation </> methodsFile
      bs = encode map
   in do B.writeFile methodDBPath bs

adjoinClassName :: ClassName -> String -> MethodName
adjoinClassName x y = x ++ "." ++ y

-- this is used in getMethods, because class names are saved like com/a/b/c/Class. We want it to be like com.a.b.c.Class
fixClassName :: String -> ClassName
fixClassName =
  map
    (\x ->
       if x == '/'
         then '.'
         else x)

type Descriptor = [(Int, Bool)] -- see `descriptorIndices` in ByteCodeParser.Reader, for how this works

type NamedMethodCode = (MethodID, [CodeAtom], CFG, [MethodAccessFlag])

findCodeAttribute :: [AttributeInfo] -> [CodeAtom]
findCodeAttribute ainfo =
  if null codes
    then []
    else (code :: AInfo -> [CodeAtom]) $ attributeInfo $ head codes
  where
    codes = filter (\at -> attributeType at == ATCode) ainfo -- there is expected to be only 1 code attribute in a method

getMethod :: ClassName -> MethodInfo -> NamedMethodCode
getMethod className methodInfo =
  let methodName :: String =
        adjoinClassName className $ name (methodInfo :: MethodInfo)
      methodDescriptor :: String = descriptorString (methodInfo :: MethodInfo)
      methodCode :: [CodeAtom] =
        findCodeAttribute $ attributes (methodInfo :: MethodInfo)
      methodCFG = generateControlFlowGraph methodCode
      methodAccessFlags = accessFlags (methodInfo :: MethodInfo)
   in trace ("Reading method: " ++ methodName) $
      ((methodName, methodDescriptor), methodCode, methodCFG, methodAccessFlags)

toMethodID :: NamedMethodCode -> MethodID
toMethodID (x, _, _, _) = x

toFieldID :: NamedField -> FieldID
toFieldID (x, _) = x

getMethods :: RawClassFile -> [NamedMethodCode]
getMethods cf = map (getMethod className) mthds
  where
    className = fixClassName $ thisClass cf
    mthds = methods cf

type NamedField = (FieldID, [FieldAccessFlag])

getField :: ClassName -> FieldInfo -> NamedField
getField className fieldInfo =
  let fieldName :: String =
        adjoinClassName className $ name (fieldInfo :: FieldInfo)
      fieldDesc :: String = descriptor (fieldInfo :: FieldInfo)
      fieldAccessFlags = accessFlags (fieldInfo :: FieldInfo)
   in trace ("Reading field: " ++ fieldName) $
      ((fieldName, fieldDesc), fieldAccessFlags)

getFields :: RawClassFile -> [NamedField]
getFields cf = map (getField className) flds
  where
    className = fixClassName $ thisClass cf
    flds = fields cf
