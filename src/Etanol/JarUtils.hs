{-# LANGUAGE OverloadedStrings, BangPatterns, MultiWayIf #-}

module Etanol.JarUtils
    ( readRawClassFilesFromPath
    , classesOnDemand
    , classesInPath
    ) where

import ByteCodeParser.BasicTypes
import ByteCodeParser.Reader
import qualified EtanolTools.Unsafe as U

import System.Mem
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified Data.Map as M
import Data.Map ((!?))

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
    
    performMajorGC

    return archive    

createClassJar :: FilePath -> IO Z.Archive
createClassJar path = do
    
    fileExistsRoutine path
        
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
        
    performMajorGC
    
    return archive    

-- NOTE: Note that extractJar and createJar are not opposites
-- Rather they unify the underlying system. extractJar produces
-- an Z.Archive from a provided JAR file, and createJar creates
-- a Z.Archive from the data in a directory.

-- And now, giveMeAnArchive abstracts both of them and returns
-- a Z.Archive whatever the input format.
giveMeAnArchive :: FilePath -> IO Z.Archive
giveMeAnArchive path = 
    if  | isJar path    -> extractJar path
        | isClass path  -> createClassJar path
        | otherwise     -> createJar  path


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

    let classNames = findClassNames archive
        result = map (\cname ->
                     --U.debugLogger ("Parsing file : " ++ cname) $
                        readRawByteString 
                            (getEntry cname archive))
                 classNames
    
    U.infoLoggerM $ "Parsed " ++ show (length result) ++ " files."

    return result

convertSlashToDot = T.map (\c -> if c == '/' then '.' else c)

classesOnDemand :: FilePath -> IO (ClassName -> Maybe RawClassFile)
classesOnDemand path = do
    archive <- giveMeAnArchive path

    let classPaths = findClassNames archive
        classNameToPath = M.fromList $
                            map (\path -> U.debugLogger ("Parsing " ++ path) $
                                    (convertSlashToDot $ thisClass $ 
                                        readRawByteString (getEntry path archive),
                                     path))
                                classPaths  
                                     
    return $ ((fmap (\a -> readRawByteString $ getEntry a archive)) . (classNameToPath !?))

classesInPath :: FilePath -> IO [ClassName]
classesInPath path = do
    archive <- giveMeAnArchive path

    let classPaths = findClassNames archive
        classNames = map (\path -> U.debugLogger ("Reading name of " ++ path) $
                                    (convertSlashToDot . thisClass . readRawByteString)
                                        (getEntry path archive))
                        classPaths
    return classNames

