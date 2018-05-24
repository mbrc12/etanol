{-# LANGUAGE DeriveGeneric, DefaultSignatures, OverloadedStrings, ScopedTypeVariables #-}

module Etanol.Types (
                getClassName, FieldType(..),
                MethodType(..), FieldName,
                MethodName, isInit, initDB,
                saveFieldDB, saveMethodDB,
                getMethodDB, getFieldDB,
                FieldDB, MethodDB
                ) where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), (!?))

import System.IO
import System.FilePath.Posix ((</>))
import System.Directory (doesFileExist)
import System.Exit (die)
import qualified Data.ByteString as B

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


