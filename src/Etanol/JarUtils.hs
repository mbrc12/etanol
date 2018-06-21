{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Etanol.JarUtils
    ( readRawClassFilesFromPath
    , createJar
    ) where

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import qualified EtanolTools.Unsafe as U

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as BL

import Data.Maybe
import Control.Monad
import Data.List

isJar :: FilePath -> Bool
isJar = isSuffixOf ".jar"

isClass :: FilePath -> Bool
isClass = isSuffixOf ".class"

toClassFileName :: FilePath -> FilePath
toClassFileName = reverse . (drop 6) . reverse

fileExistsRoutine :: FilePath -> IO ()
fileExistsRoutine path = do
    fileExists  <- doesFileExist path
    when (not fileExists) $
        die $ "File " ++ path ++ " does not exist!"

directoryExistsRoutine :: FilePath -> IO ()
directoryExistsRoutine path = do
    dirExists <- doesDirectoryExist path
    when (not dirExists) $
        die $ "Directory " ++ path ++ " does not exist!"

extractJar :: FilePath -> IO Z.Archive
extractJar path = do
    
    when (not $ isJar path) $
        die "The provided file is not a .jar file!"

    fileExistsRoutine path

    U.infoLoggerM $ "Reading " ++ path

    fileData <- BL.readFile path

    return $ Z.toArchive fileData
    

-- create a JAR file called transientJarFileName in writeTo/
createJar :: FilePath -> IO Z.Archive
createJar path = do

    directoryExistsRoutine path
    
    U.infoLoggerM $ "Reading from " ++ path

    let extraOpts = if U.getVerbosity == U.DebugLevel
                    then [Z.OptVerbose]
                    else []

    -- create Archive containing all the files in the directory
    -- recursively
    archive <- Z.addFilesToArchive 
                    (Z.OptRecursive : extraOpts)
                    Z.emptyArchive
                    [path]
 
    return archive    

-- NOTE: Note that extractJar and createJar are not opposites
-- Rather they unify the underlying system. extractJar produces
-- an Z.Archive from a provided JAR file, and createJar creates
-- a Z.Archive from the data in a directory.

-- And now, giveMeAnArchive abstracts both of them and returns
-- a Z.Archive whatever the input format.
giveMeAnArchive :: FilePath -> IO Z.Archive
giveMeAnArchive path = 
    if isJar path
    then extractJar path
    else createJar  path


findClassNames :: Z.Archive -> [FilePath]
findClassNames archive = 
    filter isClass $ 
    Z.filesInArchive archive

getEntry :: FilePath -> Z.Archive -> BL.ByteString
getEntry path archive = 
    let entry = Z.findEntryByPath path archive
    in  if isNothing entry
        then error "Unexpected exception : Entry does not exist!"
        else Z.fromEntry $ fromJust entry

readRawClassFilesFromPath :: FilePath -> IO [RawClassFile]
readRawClassFilesFromPath path = do
    
    archive <- giveMeAnArchive path
    
    U.infoLoggerM "Parsing files."

    let !classNames = findClassNames archive
        !result = map (\cname ->
                     U.debugLogger 
                        ("Parsing file : " ++ cname) $
                        readRawByteString 
                            (getEntry cname archive))
                 classNames
    
    U.infoLoggerM $ "Parsed " ++ show (length result) ++ " files."

    return result
    
