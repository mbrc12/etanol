{-# LANGUAGE OverloadedStrings, MultiWayIf, TemplateHaskell, BangPatterns #-}

module Main
    ( main
    ) where

import Etanol.Analysis
import Etanol.Driver
import Etanol.Types
import qualified Etanol.Crawler as CB
import qualified ByteCodeParser.BasicTypes as BT
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Monad
import System.Directory (canonicalizePath)
import System.Environment
import System.IO

import Options.Applicative
import Data.Semigroup
import Data.Maybe

import Development.GitRev

data Options 
    = Analyse 
        { analyse_path :: FilePath
        , sources_path :: [FilePath]
        , output_path  :: FilePath
        }
    | Benchmark { benchmark_path :: String }
    | Prettify { prettify_path :: FilePath, pre_output_path :: FilePath }
    | Version 

    deriving Show

sourceReader :: ReadM [FilePath]
sourceReader = maybeReader $ Just . words
    

performTasks :: Options -> IO ()

performTasks Version = putStrLn $
    "etanolx\nGit revision:\t" ++ $(gitHash) ++ "\n"++
    "Last commit:\t"++ $(gitCommitDate)++"\n"


performTasks (Analyse path sources output) = do
    path' <- canonicalizePath path
    sources' <- mapM canonicalizePath sources
    output' <- canonicalizePath output
    driver2 path' sources' output'

performTasks (Benchmark path) = do
    putStrLn "Benchmarking.."
    absPath <- canonicalizePath path
    !rcs <- CB.classesOnDemand absPath
    !cls <- CB.classesInPath absPath
    let cl = map (BT.thisClass . fromJust . rcs) cls
        --le = map (BT.majorVersion . fromJust . rcs) cls
    print $! cl
    --print $! le

-- Parsers begin

performTasks (Prettify path output) = do
    putStrLn $ "Outputing pretty output for " ++ path ++ " to " ++ output ++ " ..."
    absPath <- canonicalizePath path
    absOut  <- canonicalizePath output
    db <- loadAllDB absPath

    let fDB = afieldDB db
        mDB = amethodDB db
        fDB_n = afieldDB_null db
        mDB_n = amethodDB_null db

    withFile absOut WriteMode $ \handle -> do
        hPutStrLn handle "Field Purity Results: "
        showAll handle fDB
        hPutStrLn handle "======================================"
        hPutStrLn handle"Method Purity Results: "
        showAll handle mDB
        hPutStrLn handle "======================================"
        hPutStrLn handle "Field Nullability Results: "
        showAll handle fDB_n
        hPutStrLn handle "======================================"
        hPutStrLn handle "Method Nullability Results: "
        showAll handle mDB_n
        hPutStrLn handle "======================================"
        hPutStrLn handle "End of output."
                                
                                
showAll :: (Show c) => Handle -> M.Map (T.Text, T.Text) c -> IO ()
showAll handle db = mapM_ (\((x, s), v) -> 
    hPutStrLn handle $ "- " ++ T.unpack x ++ " [" ++ T.unpack s ++ "] : " ++ show v
    ) $ M.toList db

        
versionParser :: Parser Options
versionParser = flag' Version
    ( long "version"
    <> short 'v'
    <> help "Displays the version (GitHash and last Commit) and exits."
    )

analyseParser :: Parser Options
analyseParser = Analyse <$> strOption 
    (   long "analyse"
    <>  short 'a'
    <>  help "Analyse the given .jar/directory. Also look at $HOME/.etanolglobalconfig for configuration about backends."
    <>  completer (bashCompleter "file")
    <>  metavar "FILENAME/DIRECTORY"
    )
    <*> Options.Applicative.option sourceReader
    (
        long "sources"
    <>  short 's'
    <>  value []
    <>  help "Source files separated by space"
    <>  completer (bashCompleter "file")
    <>  metavar "FILENAME(s)"
    )
    <*> strOption 
    (   long "output"
    <>  short 'o'
    <>  help "Output location of the results."
    <>  completer (bashCompleter "file")
    <>  metavar "FILENAME"
    )
    

benchmarkParser :: Parser Options
benchmarkParser = Benchmark <$> strOption
    (   long "benchmark"
    <>  short 'b'
    <>  help "Benchmark by reading the files in the given directory. This is not expected to be stable."
    <>  completer (bashCompleter "directory")
    <>  metavar "DIRECTORY"
    <>  hidden
    )

prettifyParser :: Parser Options
prettifyParser = Prettify <$> strOption 
    (   long "prettify"
    <>  short 'p'
    <>  help "Prettify the outputted database file."
    <>  completer (bashCompleter "file")
    <>  metavar "DB FILEPATH"
    )
    <*> strOption 
    (   long "output"
    <>  short 'o'
    <>  help "Output path."
    <>  value "output.txt"
    <>  metavar "FILENAME"
    )

parser :: Parser Options
parser = helper <*>
        analyseParser <|> benchmarkParser <|> prettifyParser <|> versionParser

executableParser :: IO Options
executableParser = execParser $
    info parser 
     (  header "--- Etanol: A purity and nullability analysis tool for Java ---"
     <> progDesc "etanolx is a command line interface for Etanol" 
     <> fullDesc
     )

main = do
    options <- executableParser
    performTasks options 
