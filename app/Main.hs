{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Main (main) where

import Etanol.Driver
import Etanol.Analysis
import Etanol.Types

import System.Exit (die)
import System.Environment
import Control.Monad

resetArg :: String
resetArg = "reset"

analyseArg :: String
analyseArg = "analyse"

helpArg :: String
helpArg = "help"

equalsSign :: Char
equalsSign = '='

dashSign :: Char
dashSign = '-'

type Argument = (String, String)

toArg :: String -> Argument
toArg s = (z, drop (1 + length z) q)
        where   q = dropWhile (== dashSign) s
                z = takeWhile (/= equalsSign) q

execArgs :: [Argument] -> IO ()
execArgs [] = return ()
execArgs ((arg, argparam) : rest) = do

        if | arg == resetArg    -> resetConfigDirectory
           | arg == analyseArg  -> startpoint argparam
           | arg == helpArg     -> putStrLn helpMessage
           | otherwise          -> die $ "Unknown argument " ++ arg ++ ". Aborting."
        
        execArgs rest

main :: IO ()
main = do        
        putStrLn introMessage
        
        arginp <- getArgs
        let args = map toArg arginp
        
        when (null args) $ putStrLn helpMessage
           
        execArgs args
        
introMessage :: String
introMessage = "\n-- Etanol : A purity and nullability analysis tool for Java --\n"

helpMessage :: String
helpMessage = "\n" ++
              "Commands : \n"++
              "\n" ++
              " reset                           Resets the config directory listed in .etanolrc\n" ++
              "\n" ++
              " analyse=\"absolute/path/\"        Analyses recursively all classes in absolute/path/\n"++
              "                                 after loading them, and updates the database in the\n"++
              "                                 config directory in .etanolrc. Creates a new .etanolrc\n"++
              "                                 and empty database if not previously there or resetted\n"++
              "\n"++
              "                                 NOTE : Please attempt to extract and analyse the rt.jar\n"++
              "                                 file before you analyse anything else.\n"
                
