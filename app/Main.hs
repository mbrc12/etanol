{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Main
    ( main
    ) where

import Etanol.Analysis
import Etanol.Driver
import Etanol.Types

import Control.Monad
import System.Directory (canonicalizePath)
import System.Environment
import System.Exit (die)

resetArg :: String
resetArg = "reset"

analyseArg :: String
analyseArg = "analyse"

dumpArg :: String
dumpArg = "dump"

helpArg :: String
helpArg = "help"

equalsSign :: Char
equalsSign = '='

dashSign :: Char
dashSign = '-'

type Argument = (String, String)

toArg :: String -> Argument
toArg s = (z, drop (1 + length z) q)
  where
    q = dropWhile (== dashSign) s
    z = takeWhile (/= equalsSign) q

execArgs :: [Argument] -> IO ()
execArgs [] = return ()
execArgs ((arg, argparam):rest) = do
    if | arg == resetArg -> resetConfigDirectory
       | arg == analyseArg ->
           do when (null argparam) $
                  die "Please point to a resource for analysis."
              absPath <- canonicalizePath argparam
              --path <- ifJarThenExtractAndGimmeFileName absPath
              startpoint absPath
       | arg == helpArg -> putStrLn helpMessage
       | arg == dumpArg ->
           do when (null argparam) $
                  die "Please point to a directory for dumping files."
              absPath <- canonicalizePath argparam
              dumpDatabases absPath
       | otherwise -> die $ "Unknown argument " ++ arg ++ ". Aborting."
    execArgs rest

main :: IO ()
main = do
    putStrLn introMessage
    arginp <- getArgs
    let args = map toArg arginp
    when (null args) $ putStrLn helpMessage
    execArgs args

introMessage :: String
introMessage =
    "\n-- Etanol : A purity and nullability analysis tool for Java --\n"

helpMessage :: String
helpMessage =
    "\n" ++
    "Commands : \n" ++
    "\n" ++
    " reset                           Resets the config directory listed in .etanolrc\n" ++
    "\n" ++
    " analyse=\"/path/to/jar/or/directory\"\n" ++
    "                                 Analyses recursively all classes in the given path (or jar)\n" ++
    "                                 after loading them, and updates the database in the\n" ++
    "                                 config directory in .etanolrc. Creates a new .etanolrc\n" ++
    "                                 and empty database if not previously there or resetted\n" ++
    "\n" ++
    "                                 NOTE : Please attempt to extract and analyse the rt.jar\n" ++
    "                                 file before you analyse anything else.\n" ++
    "\n" ++
    " dump=\"/path/to/directory/\"\n" ++
    "                                 Dumps the results of analysis in .yaml files in the directory\n" ++
    "                                 provided in the argument.\n"
