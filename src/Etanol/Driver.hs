{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, ScopedTypeVariables #-}

module Etanol.Driver (
               Config(..), startpoint, resetConfigDirectory, ifJarThenExtractAndGimmeFileName
        ) where

import System.Directory (
                        doesDirectoryExist, 
                        doesFileExist, 
                        createDirectory, 
                        removeDirectoryRecursive,
                        findExecutable
                        )
import Control.Monad
import Data.Maybe
import Data.Either
import System.Exit (die)
import System.FilePath.Posix ((</>))
import System.Environment (lookupEnv)
import System.Process (callCommand)
import qualified GHC.Generics as G
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import qualified Data.Map as M

import Etanol.Analysis
import Etanol.Crawler
import Etanol.Types
import Etanol.Decompile
import ByteCodeParser.BasicTypes

configName :: FilePath
configName = ".etanolrc"

jarExtension, unzipCommand :: String
jarExtension = ".jar"
unzipCommand = "unzip"

data Config = Config {
                config_directory :: String
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
                putStrLn $ "No config file " ++ show configpath ++ " found. Creating default config file."
                writeFile configpath defconf
                putStrLn "Completed."
                
        
        return configpath
        
getConfigDirectory :: IO FilePath
getConfigDirectory = do
        path <- getConfigPath
        configdata <- B.readFile path
        
        let config :: Maybe Config = Y.decode configdata
        
        --print config
        
        when (isNothing config) $ error $ "Invalid config file " ++ show path ++ ". Aborting."
        
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
           then  do
                        maybeUnzip <- findExecutable unzipCommand
                        when (isNothing maybeUnzip) $ die "\"unzip\" tool could not be located. Please install it and put in on the PATH."
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
        
        putStrLn $ "Completed. " ++ (show $ length rcf) ++ " classes loaded."
        
        (ifDB, imDB) <- getInitialDBs config 
        
        putStrLn "Databases loaded."
        
        let rcs = map (\rc -> if isLeft rc 
                                then error ("ERROR : " ++ show rc)
                                else fromRight undefined rc) rcf
                
            mthds = concatMap getMethods rcs                 
            flds  = concatMap getFields  rcs    
            
            cnames = map (javaNamify . thisClass) rcs
            cps    = map constantPool rcs
            
            cmap = M.fromList $ zip cnames cps
            
            mids  = map toMethodID mthds
            fids  = map toFieldID  flds
            
            loadedThings = map (\(i, d) -> (EFieldID i, EFieldData d)) (zip fids flds) ++
                                map (\(i, d) -> (EMethodID i, EMethodData d)) (zip mids mthds)
        
            loadedStatus = map (\i -> (EFieldID i, NotAnalyzed)) fids ++ map (\i -> (EMethodID i, NotAnalyzed)) mids
            
            ltm   = M.fromList loadedThings                    
            lst   = M.fromList loadedStatus
            
            (_, ffDB, fmDB) = analyseAll cmap ltm lst ifDB imDB
        
        saveFieldDB config ffDB
        saveMethodDB config fmDB
        putStrLn "Completed."
        
startpoint :: FilePath -> IO ()
startpoint path = do
        conf <- getConfigDirectory
        driver conf path
        
