{-# LANGUAGE OverloadedStrings, DuplicateRecordFields,
  DeriveGeneric, ScopedTypeVariables #-}

module Etanol.Driver
    ( Config(..)
    , startpoint
    , resetConfigDirectory
    , ifJarThenExtractAndGimmeFileName
    , dumpDatabases
    ) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Yaml as Y
import qualified GHC.Generics as G
import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , removeDirectoryRecursive
    )
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.IO
import System.Process (callCommand)

import ByteCodeParser.BasicTypes
import Etanol.Analysis
import Etanol.Crawler
import Etanol.JarUtils
import Etanol.Decompile
import Etanol.Types

import qualified EtanolTools.Unsafe as U

fieldsYaml, methodsYaml, fieldsYaml_null, methodsYaml_null :: String
fieldsYaml = "fields.yaml"

methodsYaml = "methods.yaml"

fieldsYaml_null = "fields_nullability.yaml"

methodsYaml_null = "methods_nullability.yaml"

configName :: FilePath
configName = ".etanolrc"

jarExtension, unzipCommand :: String
jarExtension = ".jar"

unzipCommand = "unzip"

data Config = Config
    { config_directory :: String
    } deriving (Show, Eq, G.Generic)

instance Y.FromJSON Config

homeDir :: IO (Maybe FilePath)
homeDir = lookupEnv "HOME"

defaultConfigFile :: IO String
defaultConfigFile = do
    hDir <- homeDir
    return $ "config_directory : " ++ ((fromJust hDir) </> ".etanol")

getConfigPath :: IO FilePath
getConfigPath = do
    hDir <- homeDir
    when (isNothing hDir) $
        error "No HOME set! Please set your HOME environment variable."
    let home = fromJust hDir
    U.infoLoggerM $ "Home directory: " ++ home
    let configpath = home </> configName
    exs <- doesFileExist configpath
    when (not exs) $ do
        defconf <- defaultConfigFile
        U.infoLoggerM $
            "No config file " ++
            show configpath ++ " found. Creating default config file."
        writeFile configpath defconf
        U.infoLoggerM "Completed."
    return configpath

getConfigDirectory :: IO FilePath
getConfigDirectory = do
    path <- getConfigPath
    configdata <- B.readFile path
    let config :: Maybe Config = Y.decode configdata
        --print config
    when (isNothing config) $
        error $ "Invalid config file " ++ show path ++ ". Aborting."
    let confdir = config_directory $ fromJust config
    exs <- doesDirectoryExist confdir
    when (not exs) $ do
        U.infoLoggerM $ "Creating non-existent directory " ++ confdir
        createDirectory confdir
        U.infoLoggerM "Completed."
    return $ confdir

endsWith :: String -> String -> Bool
s `endsWith` t = t == reverse (take (length t) (reverse s))

ifJarThenExtractAndGimmeFileName :: FilePath -> IO FilePath
ifJarThenExtractAndGimmeFileName fp =
    if (fp `endsWith` jarExtension)
        then do
            maybeUnzip <- findExecutable unzipCommand
            when (isNothing maybeUnzip) $
                die
                    "\"unzip\" tool could not be located. Please install it and put in on the PATH."
            confDir <- getConfigDirectory
            let unzipPath = confDir </> "unzipped"
            exs <- doesDirectoryExist unzipPath
            when (exs) $ do
                U.infoLoggerM "Cleaning the previously extracted files.."
                removeDirectoryRecursive unzipPath
                U.infoLoggerM "Completed."
            let unzipShell = unzipCommand ++ " " ++ fp ++ " -d " ++ unzipPath
            U.infoLoggerM "\nExtracting jar files..\n"
            callCommand unzipShell
            U.infoLoggerM "\nDone.\n"
            return unzipPath
        else return fp

resetConfigDirectory :: IO ()
resetConfigDirectory = do
    dir <- getConfigDirectory
    U.infoLoggerM $ "Resetting config directory " ++ dir ++ ".."
    removeDirectoryRecursive dir
    U.infoLoggerM "Completed."

getInitialDBs :: FilePath -> 
                IO (FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB)
getInitialDBs config = do
    exs <- isInit config
    when (not exs) $ initDB config
    fDB <- getFieldDB config
    mDB <- getMethodDB config
    n_fDB <- getFieldDB_null config
    n_mDB <- getMethodDB_null config
    return (fDB, mDB, n_fDB, n_mDB)

driver :: FilePath -> FilePath -> IO ()
driver config path = do
    --exs <- doesDirectoryExist path
    --when (not exs) $ error "The said directory does not exist! Aborting."
    
    let backend = U.getBackend
        processingFunction = if backend == U.DirectoryBackend
                                then readRawClassFilesInDirectory
                                else readRawClassFilesFromPath

    path' <- if backend == U.DirectoryBackend   -- if directory then you need to 
                                                -- extract jar files
             then ifJarThenExtractAndGimmeFileName path
             else return path
     
    U.infoLoggerM "Reading classes.."
    
    rcs <- processingFunction path

    (ifDB, imDB, n_ifDB, n_imDB) <- getInitialDBs config
    U.infoLoggerM "Databases loaded." 
    

    let 
        mthds = concatMap getMethods rcs
        flds = concatMap getFields rcs
        cnames = map (javaNamify . thisClass) rcs
        cps = map constantPool rcs
        cmap = M.fromList $ zip cnames cps
        mids = map toMethodID mthds
        fids = map toFieldID flds
        loadedThings =
            map (\(i, d) -> (EFieldID i, EFieldData d)) (zip fids flds) ++
            map (\(i, d) -> (EMethodID i, EMethodData d)) (zip mids mthds)
        loadedStatus =
            map (\i -> (EFieldID i, NotAnalyzed)) fids ++
            map (\i -> (EMethodID i, NotAnalyzed)) mids
        ltm = M.fromList loadedThings
        lst = M.fromList loadedStatus
        (_, ffDB, fmDB, n_ffDB, n_fmDB) = analyseAll 
                                                cmap 
                                                ltm 
                                                lst 
                                                ifDB 
                                                imDB
                                                n_ifDB
                                                n_imDB
    saveFieldDB config ffDB
    saveMethodDB config fmDB
    saveFieldDB_null config n_ffDB
    saveMethodDB_null config n_fmDB
    U.infoLoggerM $
        "Completed. " ++ (show $ length rcs) ++ " classes loaded and analysed."

dumpDatabases :: FilePath -> IO ()
dumpDatabases path = do
    exsdir <- doesDirectoryExist path
    when (not exsdir) $ do
        U.infoLoggerM $ "Directory " ++ path ++ " does not exist. Create? (y/n) [n] "
        hFlush stdout
        ch <- getLine
        when (null ch) $ die "Not creating a new directory."
        if (head ch `elem` ['y', 'Y'])
            then do
                U.infoLoggerM "Creating directory.."
                createDirectory path
                U.infoLoggerM "Done."
            else die "Not creating a new directory."
    confDir <- getConfigDirectory
    initialized <- isInit confDir
    when (not initialized) $
        die
            "No fields and methods databases, please perform atleast one analysis first."
    U.infoLoggerM $ "Dumping analysis data to " ++ path
    fDB <- getFieldDB confDir
    mDB <- getMethodDB confDir
    n_fDB <- getFieldDB_null confDir
    n_mDB <- getMethodDB_null confDir
    let fieldsYamlPath = path </> fieldsYaml
        methodsYamlPath = path </> methodsYaml
        n_fieldsYamlPath = path </> fieldsYaml_null
        n_methodsYamlPath = path </> methodsYaml_null
        encFields = Y.encode fDB
        encMethods = Y.encode mDB
        n_encFields = Y.encode n_fDB
        n_encMethods = Y.encode n_mDB
    B.writeFile fieldsYamlPath encFields
    B.writeFile methodsYamlPath encMethods
    B.writeFile n_fieldsYamlPath n_encFields
    B.writeFile n_methodsYamlPath n_encMethods
    U.infoLoggerM "Completed."

startpoint :: FilePath -> IO ()
startpoint path = do
    conf <- getConfigDirectory
    driver conf path
