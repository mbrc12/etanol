{-# LANGUAGE OverloadedStrings #-}

module Etanol.Crawler
    ( readRawClassFilesFromPath
    , classesOnDemand
    , classesInPath
    , classesOnDemandBS
    ) where

-- TODO: Once this becomes stable document what all is being imported into scope
-- by each import

import System.Directory 
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    , getTemporaryDirectory
    , findExecutable
    , createDirectory
    , removeDirectoryRecursive
    )

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import Control.Applicative ((<*>), pure)
import Control.Monad (filterM, mapM, when)
import Data.List (isSuffixOf)
import Data.Map ((!?))
import Data.Maybe
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.Process (callCommand)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text as T
import qualified EtanolTools.Unsafe as U

isClass :: FilePath -> Bool
isClass = isSuffixOf ".class"

unzipCommand = "unzip"

isClassAndFile :: FilePath -> IO Bool
isClassAndFile path = do
    isFile <- doesFileExist path
    return $! (isClass path) && isFile

-- | removes the ".class" part of a class
-- toClassFileName :: FilePath -> FilePath
-- toClassFileName = reverse . (drop 6) . reverse -- 6 = length ".class"

getClassFileNames :: FilePath -> IO [FilePath]
getClassFileNames directory = do
    inside <- pure (map (directory </>)) <*> listDirectory directory
    files <- filterM isClassAndFile inside
    directories <- filterM doesDirectoryExist inside
    recursive_files <- pure concat <*> mapM getClassFileNames directories
    return $! files ++ recursive_files

readRawClassFilesFromPath :: FilePath -> IO [RawClassFile]
readRawClassFilesFromPath directory = 
    (fmap . fmap) T.pack (getClassFileNames directory) >>= mapM readRawClassFile

readCompleteFile :: FilePath -> IO BL.ByteString
readCompleteFile path = pure BL.fromStrict <*> B.readFile path

convertSlashToDot = T.map (\c -> if c == '/' then '.' else c)

isJar :: FilePath -> Bool
isJar = isSuffixOf ".jar"

adjustForJar :: FilePath -> IO FilePath
adjustForJar path = do
    tmp <- getTemporaryDirectory
    if isJar path 
    then do
        maybeUnzip <- findExecutable unzipCommand
        when (isNothing maybeUnzip) $
            die "\"unzip\" tool could not be located. Please install it and put in on the PATH."
        let unzipPath = tmp </> "etanol_tmp/"
        exs <- doesDirectoryExist unzipPath
        when (exs) $ do
                U.infoLoggerM "Cleaning the previously extracted files.."
                removeDirectoryRecursive unzipPath
                U.infoLoggerM "Completed."
        let unzipShell = unzipCommand ++ " " ++ path ++ " -d " ++ unzipPath
        U.infoLoggerM "\nExtracting jar files..\n"
        callCommand unzipShell
        U.infoLoggerM "\nDone.\n"
        return $! unzipPath
    else return $! path


loadedInMemoryDirectory :: FilePath -> IO (M.Map ClassName RawClassFile)
loadedInMemoryDirectory path = do
    path'   <- adjustForJar path
    classes <- getClassFileNames path'
    pure M.fromList <*>
                mapM (\path -> do
                                fileContents <- readCompleteFile path
                                return $! (convertSlashToDot $! thisClass $!
                                               -- U.debugLogger ("Parsing " ++ path)  $!
                                                readRawByteString fileContents, 
                                                --fileContents))
                                            readRawByteString fileContents)) 
                        classes

classesOnDemand :: FilePath -> IO (ClassName -> Maybe RawClassFile)
classesOnDemand path = do
    mapOfFiles <- loadedInMemoryDirectory path
    return (mapOfFiles !?)
    --print $ M.keys mapOfFiles
    --return $! ((fmap readRawByteString) . (mapOfFiles !?))

classesInPath :: FilePath -> IO [ClassName]
classesInPath path = do
    mp <- loadedInMemoryDirectory path 
    return $! M.keys mp

loadedInMemoryDirectoryBS :: FilePath -> IO (M.Map ClassName BL.ByteString)
loadedInMemoryDirectoryBS path = do
    path'   <- adjustForJar path
    classes <- getClassFileNames path'
    pure M.fromList <*>
                mapM (\path -> do
                                fileContents <- readCompleteFile path
                                return $! (convertSlashToDot $! thisClass $!
                                                -- U.debugLogger ("Parsing " ++ path)  $!
                                                readRawByteString fileContents, 
                                                --fileContents))
                                            fileContents)) 
                        classes

classesOnDemandBS :: FilePath -> IO (ClassName -> Maybe BL.ByteString)
classesOnDemandBS path = do
    mapOfFiles <- loadedInMemoryDirectoryBS path
    return (mapOfFiles !?)
    --print $ M.keys mapOfFiles
    --return $! ((fmap readRawByteString) . (mapOfFiles !?))
