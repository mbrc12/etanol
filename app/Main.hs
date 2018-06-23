{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Main
    ( main
    ) where

import Etanol.Analysis
import Etanol.Driver
import Etanol.Types
import Etanol.Crawler

import Control.Monad
import System.Directory (canonicalizePath)
import System.Environment

import Options.Applicative
import Data.Semigroup

data Options 
    = Reset 
    | Analyse { analyse_path :: String } 
    | Dump { dump_path :: String }
    | Benchmark { benchmark_path :: String }

    deriving Show

performTasks :: Options -> IO ()
performTasks Reset = resetConfigDirectory

performTasks (Analyse path) = do
    absPath <- canonicalizePath path
    startpoint absPath

performTasks (Dump path) = do
    absPath <- canonicalizePath path
    dumpDatabases absPath

performTasks (Benchmark path) = do
    absPath <- canonicalizePath path
    files <- readRawClassFilesInDirectory absPath
    print $! files

-- Parsers begin

resetParser :: Parser Options
resetParser = flag' Reset 
    (   long "reset"
    <>  short 'r'
    <>  help "Reset the database."
    )

analyseParser :: Parser Options
analyseParser = Analyse <$> strOption 
    (   long "analyse"
    <>  short 'a'
    <>  help "Analyse the given .jar/directory. Also look at $HOME/.etanolglobalconfig for configuration about backends."
    <>  completer (bashCompleter "file")
    <>  metavar "FILENAME/DIRECTORY"
    )

dumpParser :: Parser Options
dumpParser = Dump <$> strOption 
    (   long "dump"
    <>  short 'd'
    <>  help "Dump the databases into the provided directory."
    <>  completer (bashCompleter "directory")
    <>  metavar "DIRECTORY"
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

parser :: Parser Options
parser = helper <*>
        resetParser <|> analyseParser <|> dumpParser <|> benchmarkParser

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
