{-# LANGUAGE RecordWildCards, LambdaCase, ViewPatterns, OverloadedStrings #-}

import Etanol.API
import Etanol.Crawler

import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad

import Data.Maybe
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T


-- This is where all the tests go----------------------

tests :: [TestUnit]
tests = []






-------------------------------------------------------
data Unit
    = UField
    { field :: FieldID
    , fpurity :: FieldType
    , fnullability :: FieldNullabilityType
    } 
    | UMethod
    { method :: MethodID
    , mpurity :: MethodType
    , mnullability :: MethodNullabilityType
    } deriving (Show)


data TestUnit 
    = TestUnit 
    { className :: T.Text 
    , units   :: [Unit]
    } deriving (Show)



testDir :: FilePath
testDir = "./test/Tests"

verify :: (Show a, Show b) => String -> a -> b -> b -> String
verify ty (show -> f) (show -> s) (show -> t) = 
    if s == t 
       then ""
       else "\n    Type: " ++ ty ++ "\n      For " ++ f ++ " . Expected: " ++ s ++ "; Found: " ++ t

check :: AnalysisOutputType -> Unit -> String
check AnalysisOutputType{..} unit = 
    case unit of
      UField fld fp fn  -> 
          verify "Field Purity" (fst fld) (Just fp) (fieldPurity fld) ++
              verify "Field Nullability" (fst fld) (Just fn) (fieldNullability fld)
      UMethod mtd mp mn  -> 
          verify "Method Purity" (fst mtd) (Just mp) (methodPurity mtd) ++
              verify "Method Nullability" (fst mtd) (Just mn) (methodNullability mtd)
  
     
test :: TestUnit -> IO ()
test TestUnit{..} = do 
    prov <- classesOnDemandBS testDir
    let classes = [className]
        targ = map (\case 
                    UField fld fp fn -> EFieldID fld 
                    UMethod mtd mp mn -> EMethodID mtd
                ) units
        
        ainput = AnalysisInputType 
                { classes = classes
                , targets = targ
                , classProvider = prov
                , sourceFieldDB = M.empty
                , sourceMethodDB = M.empty
                , sourceFieldNullabilityDB = M.empty
                , sourceMethodNullabilityDB = M.empty
                }

        result = analysis ainput

        output = fromRight (error "Classes not found!") result

        checks = concatMap (check output) units

    unless (null checks) $
        error $ "\nTests failed, please check: \n" ++ checks




main :: IO ()
main = mapM_ test tests 

{--

main :: IO ()
main = do 
    tests <- map (testDir </>) <$> listDirectory testDir
    results <- mapM test tests 

    unless (all isNothing results) $ do
        fcount <- mapM (\path -> do
                    when (isJust path) $ 
                        putStrLn $ "Failed test : " ++ fromJust path
                    if isNothing path
                       then return 0
                       else return 1
                    ) results
        let s = sum fcount

        error $ show s ++ " / " ++ show (length fcount) ++ " TESTS FAILED"   
--}

