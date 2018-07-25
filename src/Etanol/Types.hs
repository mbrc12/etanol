{-# LANGUAGE DeriveGeneric, DefaultSignatures, OverloadedStrings,
  ScopedTypeVariables, DuplicateRecordFields, BangPatterns #-}

{- Etanol.Types. Includes basic Database functionality for Etanol field 
and method data. Also includes helper functions for Etanol.Analysis -}

module Etanol.Types
    ( Descriptor(..)
    , getClassName
    , AnyID(..)
    , anyIDToClassName
    , FieldType(..)
    , MethodType(..)
    , FieldNullabilityType(..)
    , MethodNullabilityType(..)
    , MethodNullabilityDB(..)
    , FieldNullabilityDB(..)
    , FieldName
    , MethodName
    , isInit
    , initDB
    , saveFieldDB
    , saveMethodDB
    , getMethodDB
    , getFieldDB
    , saveFieldDB_null
    , saveMethodDB_null
    , getMethodDB_null
    , getFieldDB_null
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
    , AllDB(..)
    , loadAllDB
    , saveAllDB
    ) where

import qualified Data.Vector as V 
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Map.Strict ((!), (!?))

import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.IO

import ByteCodeParser.BasicTypes hiding (ClassName)
import ByteCodeParser.Instructions
import ByteCodeParser.Reader

import Etanol.ControlFlowGraph

import Data.Serialize
import Data.Serialize.Text

import qualified Data.Set as S

import qualified Data.Yaml as Y
import GHC.Generics

import qualified EtanolTools.Unsafe as U
import Control.Monad

unsafeHead :: String -> [a] -> a
unsafeHead err xs =
    if null xs
        then (error err)
        else (head xs)

getClassName :: T.Text -> T.Text
getClassName = T.reverse . T.tail . T.dropWhile (/= '.') . T.reverse

fieldsFile, methodsFile, fieldsNullFile, methodsNullFile :: FilePath
fieldsFile = "fields.db"

methodsFile = "methods.db"

fieldsNullFile = "fields_nullability.db"

methodsNullFile = "methods_nullability.db"

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

type FieldDescriptor = T.Text

type MethodDescriptor = T.Text

type FieldName = T.Text

type MethodName = T.Text

type ClassName = T.Text

type FieldID = (FieldName, FieldDescriptor)

type MethodID = (MethodName, MethodDescriptor)

data AnyID
    = EFieldID { fieldID :: !FieldID }
    | EMethodID { methodID :: !MethodID }
    deriving (Eq, Ord)

instance Show AnyID where
    show (EFieldID f) = "field " ++ T.unpack (fst f) ++ ":" ++ T.unpack (snd f)
    show (EMethodID m) = "method " ++ T.unpack (fst m) ++ ":" ++ T.unpack (snd m)

anyIDToClassName :: AnyID -> ClassName
anyIDToClassName (EFieldID (fieldName, _)) = getClassName fieldName
anyIDToClassName (EMethodID (methodName, _)) = getClassName methodName

data FieldType
    = Normal
    | Basic
    | FinalStatic
    | UnanalyzableField
    deriving (Show, Eq, Ord, Generic)

data FieldNullabilityType
    = NullableField
    | NonNullableField
    | UndecidedField
    | UnanalyzableNullField
    deriving (Show, Eq, Ord, Generic)

instance Serialize FieldType -- heavylifting done by deriving Generic 

instance Serialize FieldNullabilityType

instance Y.FromJSON FieldType

instance Y.ToJSON FieldType

instance Y.FromJSON FieldNullabilityType

instance Y.ToJSON FieldNullabilityType

data MethodType
    = Pure
    | Impure
    | Local
    | StrongImpure
    | UnanalyzableMethod
    deriving (Show, Eq, Ord, Generic)

data MethodNullabilityType
    = NullableMethod
    | NonNullableMethod
    | UndecidedMethod
    | UnanalyzableNullMethod
    deriving (Show, Eq, Ord, Generic)

instance Serialize MethodType

instance Serialize MethodNullabilityType

instance Y.FromJSON MethodType

instance Y.ToJSON MethodType

instance Y.FromJSON MethodNullabilityType

instance Y.ToJSON MethodNullabilityType

type FieldDB = M.Map FieldID FieldType

type MethodDB = M.Map MethodID MethodType

type FieldNullabilityDB = M.Map FieldID FieldNullabilityType

type MethodNullabilityDB = M.Map MethodID MethodNullabilityType

data AllDB = AllDB 
        { afieldDB       :: FieldDB
        , amethodDB      :: MethodDB
        , afieldDB_null  :: FieldNullabilityDB
        , amethodDB_null :: MethodNullabilityDB
        , aclasses       :: S.Set ClassName
        } deriving (Show, Generic)

instance Serialize AllDB

loadAllDB :: FilePath -> IO AllDB
loadAllDB path = do
    exs <- doesFileExist path
    when (not $ exs) $
        die $ "File " ++ path ++ " does not exist!"
    bs <- B.readFile path
    let dec = decode bs
    case dec of 
        Right alldb -> return alldb
        Left  _     -> die "Invalid database file!"

saveAllDB :: FilePath -> AllDB -> IO ()
saveAllDB path db = do
    B.writeFile path (encode db)
    U.debugLoggerM $ "Written database to " ++ path

isInit :: FilePath -> IO Bool
isInit configLocation =
    let fieldDBPath = configLocation </> fieldsFile
        methodDBPath = configLocation </> methodsFile
        n_fieldDBPath = configLocation </> fieldsNullFile
        n_methodDBPath = configLocation </> methodsNullFile

     in do finit <- doesFileExist fieldDBPath
           minit <- doesFileExist methodDBPath
           nfinit <- doesFileExist n_fieldDBPath
           nminit <- doesFileExist n_methodDBPath
           return $ finit && minit && nfinit && nminit

initDB :: FilePath -> IO ()
initDB configLocation =
    let fieldDBpath = configLocation </> fieldsFile
        methodDBpath = configLocation </> methodsFile
        n_methodDBpath = configLocation </> methodsNullFile
        n_fieldDBpath = configLocation </> fieldsNullFile
        fieldS = encode (M.empty :: FieldDB)
        methodS = encode (M.empty :: MethodDB)
        n_fieldS = encode (M.empty :: FieldNullabilityDB)
        n_methodS = encode (M.empty :: MethodNullabilityDB)
     in do B.writeFile fieldDBpath fieldS
           B.writeFile methodDBpath methodS
           B.writeFile n_fieldDBpath n_fieldS
           B.writeFile n_methodDBpath n_methodS

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

getFieldDB_null :: FilePath -> IO FieldNullabilityDB
getFieldDB_null configLocation =
    let n_fieldDBPath = configLocation </> fieldsNullFile
     in do bs <- B.readFile n_fieldDBPath
           let dec = decode bs
           case dec of
               Right map -> return map
               Left _ -> die "Invalid field database file!"

getMethodDB_null :: FilePath -> IO MethodNullabilityDB
getMethodDB_null configLocation =
    let n_methodDBPath = configLocation </> methodsNullFile
     in do bs <- B.readFile n_methodDBPath
           let dec = decode bs
           case dec of
               Right map -> return map
               Left _ -> die "Invalid method database file!"

saveFieldDB_null :: FilePath -> FieldNullabilityDB -> IO ()
saveFieldDB_null configLocation map =
    let n_fieldDBPath = configLocation </> fieldsNullFile
        bs = encode map
     in do B.writeFile n_fieldDBPath bs

saveMethodDB_null :: FilePath -> MethodNullabilityDB -> IO ()
saveMethodDB_null configLocation map =
    let n_methodDBPath = configLocation </> methodsNullFile
        bs = encode map
     in do B.writeFile n_methodDBPath bs




adjoinClassName :: ClassName -> T.Text -> MethodName
adjoinClassName x y = T.concat [x, ".", y]

-- this is used in getMethods, because class names are saved like com/a/b/c/Class. We want it to be like com.a.b.c.Class
fixClassName :: T.Text -> ClassName
fixClassName =
    T.map (\x ->
             if x == '/'
                 then '.'
                 else x)

type Descriptor = [(Int, Bool)] -- see `descriptorIndices` in ByteCodeParser.Reader, for how this works

type NamedMethodCode = (MethodID, V.Vector CodeAtom, CFG, [MethodAccessFlag])

findCodeAttribute :: [AttributeInfo] -> V.Vector CodeAtom
findCodeAttribute ainfo =
    if null codes
        then V.empty
        else (code :: AInfo -> V.Vector CodeAtom) $ attributeInfo $ head codes
  where
    codes = filter (\at -> attributeType at == ATCode) ainfo -- there is expected to be only 1 code attribute in a method

getMethod :: ClassName -> MethodInfo -> NamedMethodCode
getMethod className methodInfo =
    let !methodName =
            adjoinClassName className $! name (methodInfo :: MethodInfo) :: T.Text
        !methodDescriptor = descriptorString (methodInfo :: MethodInfo) :: T.Text
        !methodCode  =
            findCodeAttribute $! attributes (methodInfo :: MethodInfo) :: V.Vector CodeAtom
        !methodCFG = generateControlFlowGraph (V.toList methodCode)
        !methodAccessFlags = accessFlags (methodInfo :: MethodInfo)
     in U.debugLogger ("Reading method: " ++ T.unpack methodName) $
        ( (methodName, methodDescriptor)
        , methodCode
        , methodCFG
        , methodAccessFlags)

toMethodID :: NamedMethodCode -> MethodID
toMethodID (x, _, _, _) = x

toFieldID :: NamedField -> FieldID
toFieldID (x, _) = x

getMethods :: RawClassFile -> [NamedMethodCode]
getMethods cf = map (getMethod className) mthds
  where
    className = fixClassName $! thisClass cf
    mthds = methods cf

type NamedField = (FieldID, [FieldAccessFlag])

getField :: ClassName -> FieldInfo -> NamedField
getField className fieldInfo =
    let !fieldName  =
            adjoinClassName className $! name (fieldInfo :: FieldInfo) :: T.Text
        !fieldDesc = descriptor (fieldInfo :: FieldInfo) :: T.Text
        !fieldAccessFlags = accessFlags (fieldInfo :: FieldInfo)
     in U.debugLogger ("Reading field: " ++ T.unpack fieldName) $
        ((fieldName, fieldDesc), fieldAccessFlags)

getFields :: RawClassFile -> [NamedField]
getFields cf = map (getField className) flds
  where
    className = fixClassName $! thisClass cf
    flds = fields cf
