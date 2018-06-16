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
import Etanol.Decompile
import Etanol.Types

fieldsYaml, methodsYaml :: String
fieldsYaml = "fields.yaml"

methodsYaml = "methods.yaml"

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
    putStrLn $ "Home directory: " ++ home
    let configpath = home </> configName
    exs <- doesFileExist configpath
    when (not exs) $ do
        defconf <- defaultConfigFile
        putStrLn $
            "No config file " ++
            show configpath ++ " found. Creating default config file."
        writeFile configpath defconf
        putStrLn "Completed."
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
        putStrLn $ "Creating non-existent directory " ++ confdir
        createDirectory confdir
        putStrLn "Completed."
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
                putStrLn "Cleaning the previously extracted files.."
                removeDirectoryRecursive unzipPath
                putStrLn "Completed."
            let unzipShell = unzipCommand ++ " " ++ fp ++ " -d " ++ unzipPath
            putStrLn "\nExtracting jar files..\n"
            callCommand unzipShell
            putStrLn "\nDone.\n"
            return unzipPath
        else return fp

resetConfigDirectory :: IO ()
resetConfigDirectory = do
    dir <- getConfigDirectory
    putStrLn $ "Resetting config directory " ++ dir ++ ".."
    removeDirectoryRecursive dir
    putStrLn "Completed."

getInitialDBs :: FilePath -> IO (FieldDB, MethodDB)
getInitialDBs config = do
    exs <- isInit config
    when (not exs) $ initDB config
    fDB <- getFieldDB config
    mDB <- getMethodDB config
    return (fDB, mDB)

driver :: FilePath -> FilePath -> IO ()
driver config path = do
    exs <- doesDirectoryExist path
    when (not exs) $ error "The said directory does not exist! Aborting."
    putStrLn "Reading classes.."
    rcf <- readRawClassFilesInDirectory path
    (ifDB, imDB) <- getInitialDBs config
    putStrLn "Databases loaded."
    let rcs =
            map
                (\rc ->
                     if isLeft rc
                         then error ("ERROR : " ++ show rc)
                         else fromRight undefined rc)
                rcf
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
        (_, ffDB, fmDB) = analyseAll cmap ltm lst ifDB imDB
    saveFieldDB config ffDB
    saveMethodDB config fmDB
    putStrLn $
        "Completed. " ++ (show $ length rcf) ++ " classes loaded and analysed."

dumpDatabases :: FilePath -> IO ()
dumpDatabases path = do
    exsdir <- doesDirectoryExist path
    when (not exsdir) $ do
        putStr $ "Directory " ++ path ++ " does not exist. Create? (y/n) [n] "
        hFlush stdout
        ch <- getLine
        when (null ch) $ die "Not creating a new directory."
        if (head ch `elem` ['y', 'Y'])
            then do
                putStrLn "Creating directory.."
                createDirectory path
                putStrLn "Done."
            else die "Not creating a new directory."
    confDir <- getConfigDirectory
    initialized <- isInit confDir
    when (not initialized) $
        die
            "No fields and methods databases, please perform atleast one analysis first."
    putStrLn $ "Dumping analysis data to " ++ path
    fDB <- getFieldDB confDir
    mDB <- getMethodDB confDir
    let fieldsYamlPath = path </> fieldsYaml
        methodsYamlPath = path </> methodsYaml
        encFields = Y.encode fDB
        encMethods = Y.encode mDB
    B.writeFile fieldsYamlPath encFields
    B.writeFile methodsYamlPath encMethods
    putStrLn "Completed."

startpoint :: FilePath -> IO ()
startpoint path = do
    conf <- getConfigDirectory
    driver conf path
