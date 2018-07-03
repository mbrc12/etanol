{-# LANGUAGE OverloadedStrings, DuplicateRecordFields,
  DeriveGeneric, ScopedTypeVariables, BangPatterns #-}

module Etanol.Driver
    ( Config(..)
    , startpoint
    , resetConfigDirectory
    , dumpDatabases
    , classDependencies
    , classDependencyPools
    , driver2
    ) where

import Data.List
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
    , getHomeDirectory
    )
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.IO
import System.Process (callCommand)

import ByteCodeParser.BasicTypes
import Etanol.Analysis
import qualified Etanol.Crawler  as CB
import qualified Etanol.JarUtils as JB
import Etanol.Decompile
import Etanol.Types
import Etanol.Utils

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Vector as V

import qualified EtanolTools.Unsafe as U


classesInPath = if U.getBackend == U.DirectoryBackend
                then CB.classesInPath
                else JB.classesInPath

readRawClassFilesFromPath = if U.getBackend == U.DirectoryBackend
                            then CB.readRawClassFilesFromPath
                            else JB.readRawClassFilesFromPath

classesOnDemand = if U.getBackend == U.DirectoryBackend
                  then CB.classesOnDemand
                  else JB.classesOnDemand

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

homeDir :: IO FilePath
homeDir = getHomeDirectory

defaultConfigFile :: IO String
defaultConfigFile = do
    hDir <- homeDir
    return $ "config_directory : " ++ ((hDir) </> ".etanol")

getConfigPath :: IO FilePath
getConfigPath = do
    hDir <- homeDir
    let home = hDir
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

{--
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
--}
    
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

classConstantPools :: FilePath 
                    -> IO (ClassName -> Maybe (V.Vector ConstantInfo))
classConstantPools path = classesOnDemand path >>= return . ((fmap constantPool) .)
                    ---            ignore this pointfree gymnastics please ^ (^.^)

classDependencies :: FilePath -> IO ([ClassName] -> [ClassName])
classDependencies path = do
    constPoolFunction <- classConstantPools path
    return $ resolver constPoolFunction

classDependencyPools :: FilePath -> IO ([ClassName] -> [[ClassName]])
classDependencyPools path = do
    constPoolFunction <- classConstantPools path
    return $ genDependencyPools constPoolFunction

{--
driver :: FilePath -> FilePath -> IO ()
driver config path = do
    --exs <- doesDirectoryExist path
    --when (not exs) $ error "The said directory does not exist! Aborting."
    {-- 
    let backend = U.getBackend
        processingFunction = if backend == U.DirectoryBackend
                                then readRawClassFilesInDirectory
                                else readRawClassFilesFromPath
        demandStream       = if backend == U.DirectoryBackend
                                then 

    path' <- if backend == U.DirectoryBackend   -- if directory then you need to 
                                                -- extract jar files
             then ifJarThenExtractAndGimmeFileName path
             else return path
    --}
    
    U.infoLoggerM "Reading classes.."
      
    rcs <- readRawClassFilesFromPath path 

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

--}

driver2 :: FilePath -> [FilePath] -> FilePath -> IO ()
driver2 path sources output = do

    allDBs <- mapM loadAllDB sources
    let merge = foldl' M.union M.empty
        fDB   = merge $ map afieldDB allDBs
        mDB   = merge $ map amethodDB allDBs
        fDB_n = merge $ map afieldDB_null allDBs
        mDB_n = merge $ map amethodDB_null allDBs
    
    cls <- classesInPath path
    depf <- classDependencyPools path
    let scc = depf cls 

    -- the equation here is: concatMap id scc == cls

    U.assertCheck (sort (concatMap id scc) == sort cls) 
        "sort (concatMap id scc) /= sort cls"

    cpoolf <- classConstantPools path
    rcf <- classesOnDemand path
    
    -- the equation here is: all isJust $ map rcf cls 
    
    U.assertCheck (all isJust $ map rcf cls) 
        "all isJust $ map rcf cls /= True"

    let (fDB', mDB', fDB_n', mDB_n') = feedForward scc rcf cpoolf
                                        (fDB, mDB, fDB_n, mDB_n)
    
    let f              = fromJust . rcf
        currentFields  = concatMap ((map toFieldID) . getFields . f)
                            cls
        currentMethods = concatMap ((map toMethodID) . getMethods . f)
                            cls

        subMap m k     = M.fromList $!
                            map (\e -> e `seq` (e, m ! e)) k

        curfDB         = subMap fDB'   currentFields
        curmDB         = subMap mDB'   currentMethods
        curfDB_n       = subMap fDB_n' currentFields
        curmDB_n       = subMap mDB_n' currentMethods

    U.debugLoggerM $ "Analysing and saving results to " ++ output
       
    putStrLn $ showSummary curfDB curmDB curfDB_n curmDB_n
    
    when (U.getVerbosity == U.DebugLevel) $ do
       print curfDB
       print curmDB
       print curfDB_n
       print curmDB_n

    saveAllDB output $! AllDB 
        { afieldDB = curfDB
        , amethodDB = curmDB
        , afieldDB_null = curfDB_n
        , amethodDB_null = curmDB_n
        }


feedForward :: [[ClassName]]
            -> (ClassName -> Maybe RawClassFile)
            -> (ClassName -> Maybe (V.Vector ConstantInfo))
            -> (FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB)
            -> (FieldDB, MethodDB, FieldNullabilityDB, MethodNullabilityDB)
feedForward [] _ _ dbs = dbs
feedForward (!comp : rest) rcf cpoolf (fDB, mDB, fDB_n, mDB_n) =
    let f = fromJust . rcf
        !mthds = concatMap (getMethods . f) comp
        !flds  = concatMap (getFields . f)  comp
        !cnames = map (javaNamify . thisClass . f) comp
        !mids = map toMethodID mthds
        !fids = map toFieldID flds
        !loadedThings = M.fromList $!
                map (\(i, d) -> (EFieldID i, EFieldData d)) (zip fids flds) ++
                map (\(i, d) -> (EMethodID i, EMethodData d)) (zip mids mthds)
        !loadedStatus = M.fromList $!
                map (\i -> (EFieldID i, NotAnalyzed)) fids ++
                map (\i -> (EMethodID i, NotAnalyzed)) mids
        (_, !fDB', !mDB', !fDB_n', !mDB_n') = analyseAll 
                                            cpoolf
                                            loadedThings
                                            loadedStatus
                                            fDB
                                            mDB
                                            fDB_n
                                            mDB_n

    in feedForward rest rcf cpoolf (fDB', mDB', fDB_n', mDB_n')
    

summarizer :: (Eq b, Ord b, Show b) => [(a, b)] -> String
summarizer xs = 
     concat $ 
        intersperse "\n" $ 
            map (\(c, g) -> "  " ++ show g ++ ": " ++ show c) $
                map (\g -> (length g, (snd $ head g))) $
                    groupBy (\x y -> snd x == snd y) $
                        sortBy (\(_, u) (_, v) -> compare u v) $
                            xs
                        
showSummary :: FieldDB 
            -> MethodDB 
            -> FieldNullabilityDB 
            -> MethodNullabilityDB 
            -> String
showSummary fDB mDB fDB_n mDB_n = 
    let lfdb = M.toList fDB
        lmdb = M.toList mDB
        lfdb_n = M.toList fDB_n
        lmdb_n = M.toList mDB_n
        fsum   = summarizer lfdb
        msum   = summarizer lmdb
        fnsum  = summarizer lfdb_n
        mnsum  = summarizer lmdb_n
    in  "Analysis results:\n\n" ++
        "Fields:\n" ++ fsum ++ "\n" ++
        "Methods:\n" ++ msum ++ "\n" ++
        "Field Nullability:\n" ++ fnsum ++ "\n" ++
        "Method Nullability:\n" ++ mnsum ++ "\n"
     


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
startpoint = undefined
{--startpoint path = do
    conf <- getConfigDirectory
    driver conf path
--}
