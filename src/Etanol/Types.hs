{-# LANGUAGE DeriveGeneric, DefaultSignatures, OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields #-}

{- Etanol.Types. Includes basic Database functionality for Etanol field and method data.
Also includes helper functions for Etanol.Analysis
-}

module Etanol.Types (
                getClassName, FieldType(..),
                MethodType(..), FieldName,
                MethodName, isInit, initDB,
                saveFieldDB, saveMethodDB,
                getMethodDB, getFieldDB,
                FieldDB(..), MethodDB(..),
                NamedMethodCode(..),
                getMethods
                ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), (!?))

import System.IO
import System.FilePath.Posix ((</>))
import System.Directory (doesFileExist)
import System.Exit (die)
import qualified Data.ByteString as B

import ByteCodeParser.BasicTypes hiding (ClassName)
import ByteCodeParser.Reader
import ByteCodeParser.Instructions

import Etanol.ControlFlowGraph

import GHC.Generics
import Data.Serialize

getClassName :: String -> String
getClassName = reverse . tail . dropWhile (/= '.') . reverse

fieldsFile, methodsFile :: FilePath
fieldsFile = "fields.db"
methodsFile = "methods.db"


type FieldName = String
type MethodName = String
type ClassName = String

data FieldType = Normal | FinalStatic 
                        deriving (Show, Eq, Generic)
instance Serialize FieldType 

data MethodType = Pure | Impure | StrongImpure
                        deriving (Show, Eq, Generic)
instance Serialize MethodType

type FieldDB = M.Map FieldName FieldType
type MethodDB = M.Map MethodName MethodType

isInit :: FilePath -> IO Bool
isInit configLocation = let     fieldDBPath = configLocation </> fieldsFile
                                methodDBPath = configLocation </> methodsFile
                        in do
                                finit <- doesFileExist fieldDBPath
                                minit <- doesFileExist methodDBPath
                                return $ finit && minit
                                

initDB :: FilePath -> IO ()
initDB configLocation = let     fieldDBpath = configLocation </> fieldsFile
                                methodDBpath = configLocation </> methodsFile
                                fieldS = encode (M.empty :: FieldDB)
                                methodS = encode (M.empty :: MethodDB)
                        in do
                                B.writeFile fieldDBpath fieldS
                                B.writeFile methodDBpath methodS

getFieldDB :: FilePath -> IO FieldDB
getFieldDB configLocation = let fieldDBPath = configLocation </> fieldsFile
                            in do
                                bs <- B.readFile fieldDBPath
                                let dec = decode bs
                                case dec of
                                        Right map -> return map
                                        Left  _   -> die "Invalid field database file!"

getMethodDB :: FilePath -> IO MethodDB
getMethodDB configLocation = let methodDBPath = configLocation </> methodsFile
                             in do
                                bs <- B.readFile methodDBPath
                                let dec = decode bs
                                case dec of
                                        Right map -> return map
                                        Left  _   -> die "Invalid method database file!"

saveFieldDB :: FilePath -> FieldDB -> IO ()
saveFieldDB configLocation map = let    fieldDBPath = configLocation </> fieldsFile
                                        bs = encode map
                                 in do
                                        B.writeFile fieldDBPath bs

                                        
saveMethodDB :: FilePath -> MethodDB -> IO ()
saveMethodDB configLocation map = let   methodDBPath = configLocation </> methodsFile
                                        bs = encode map
                                  in do
                                        B.writeFile methodDBPath bs


adjoinClassName :: ClassName -> String -> MethodName
adjoinClassName x y = x ++ "." ++ y

-- this is used in getMethods, because class names are saved like com/a/b/c/Class. We want it to be like com.a.b.c.Class
fixClassName :: String -> ClassName
fixClassName =  map (\x -> if x == '/' then '.' else x)

type Descriptor = [(Int, Bool)] -- see `descriptorIndices` in ByteCodeParser.Reader, for how this works
type NamedMethodCode = (MethodName, Descriptor, CFG)

findCodeAttribute :: [AttributeInfo] -> [CodeAtom]
findCodeAttribute ainfo =  (code :: AInfo -> [CodeAtom]) $
                                attributeInfo $ 
                                        head $ filter (\at -> attributeType at == ATCode) ainfo -- there is expected to be only 1 code attribute in a method

getMethod :: ClassName -> MethodInfo -> NamedMethodCode
getMethod className methodInfo = let methodName :: String = adjoinClassName className $ name (methodInfo :: MethodInfo)
                                     methodDescriptor :: Descriptor = descriptor (methodInfo :: MethodInfo)
                                     methodCode :: [CodeAtom] = findCodeAttribute $ attributes (methodInfo :: MethodInfo)
                                     methodCFG = generateControlFlowGraph methodCode
                                 in (methodName, methodDescriptor, methodCFG)


getMethods :: RawClassFile -> [NamedMethodCode]
getMethods cf = map (getMethod className) mthds
                where
                        className = fixClassName $ thisClass cf
                        mthds     = methods cf
