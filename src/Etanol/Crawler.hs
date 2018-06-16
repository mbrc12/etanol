{-# LANGUAGE OverloadedStrings #-}

module Etanol.Crawler
    ( getClassFileNames
    , readRawClassFilesInDirectory
    ) where

-- TODO: Once this becomes stable document what all is being imported into scope
-- by each import
import ByteCodeParser.BasicTypes (Error, RawClassFile)
import ByteCodeParser.Reader (readRawClassFile)
import Control.Applicative ((<*>), pure)
import Control.Monad (filterM, mapM)
import Data.List (isSuffixOf)
import Debug.Trace (trace)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix ((</>))

isClass :: FilePath -> Bool
isClass = isSuffixOf ".class"

isClassAndFile :: FilePath -> IO Bool
isClassAndFile path = do
    isFile <- doesFileExist path
    return $ (isClass path) && isFile

-- | removes the ".class" part of a class
toClassFileName :: FilePath -> FilePath
toClassFileName = reverse . (drop 6) . reverse -- 6 = length ".class"

getClassFileNames :: FilePath -> IO [FilePath]
getClassFileNames directory = do
    inside <- pure (map (directory </>)) <*> listDirectory directory
    files <- pure (map toClassFileName) <*> filterM isClassAndFile inside
    directories <- filterM doesDirectoryExist inside
    recursive_files <- pure concat <*> mapM getClassFileNames directories
    return $ files ++ recursive_files

readRawClassFilesInDirectory :: FilePath -> IO [Either Error RawClassFile]
readRawClassFilesInDirectory directory =
    getClassFileNames directory >>= mapM readRawClassFile
