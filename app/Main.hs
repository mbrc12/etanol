{-# LANGUAGE OverloadedStrings, MultiWayIf, TemplateHaskell, BangPatterns #-}

module Main
    ( main
    ) where

import Etanol.Analysis
import Etanol.Driver
import Etanol.Types
import qualified Etanol.Crawler as CB
import qualified ByteCodeParser.BasicTypes as BT

import Control.Monad
import System.Directory (canonicalizePath)
import System.Environment

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

parser :: Parser Options
parser = helper <*>
        analyseParser <|> benchmarkParser <|> versionParser

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
