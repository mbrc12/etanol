{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, ScopedTypeVariables #-}

module Etanol.Driver (
               Config(..), startpoint
        ) where

import System.Directory (doesDirectoryExist, doesFileExist, createDirectory)
import Control.Monad
import Data.Maybe
import Data.Either 
import System.FilePath.Posix ((</>))
import System.Environment (lookupEnv)
import qualified GHC.Generics as G
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import qualified Data.Map as M

import Etanol.Analysis
import Etanol.Crawler
import Etanol.Types
import ByteCodeParser.BasicTypes

configName :: FilePath
configName = ".etanolrc"

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
        
        let configpath = (fromJust hDir) </> configName   
            
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
        
        print config
        
        when (isNothing config) $ error $ "Invalid config file " ++ show path ++ ". Aborting."
        
        let confdir = config_directory $ fromJust config
        
        exs <- doesDirectoryExist confdir
        
        when (not exs) $ do
                putStrLn $ "Creating non-existent directory " ++ confdir
                createDirectory confdir
                putStrLn "Completed."
        
        return $ confdir

getInitialDBs :: FilePath -> IO (FieldDB, MethodDB)
getInitialDBs config = do
        
        exs <- isInit config
        when (not exs) $ initDB config 
        
        fDB <- getFieldDB config
        mDB <- getMethodDB config
        
        return (fDB, mDB)
        
driver :: FilePath -> FilePath -> IO (FieldDB, MethodDB)
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

            cnames = map thisClass rcs
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
            
        return (ffDB, fmDB)    
        
startpoint :: FilePath -> IO (FieldDB, MethodDB)
startpoint path = do
        conf <- getConfigDirectory
        driver conf path
        
