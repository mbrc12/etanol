{-# LANGUAGE RecordWildCards, LambdaCase, ViewPatterns, OverloadedStrings #-}

import Etanol.API
import Etanol.Crawler
import Etanol.Types

import System.Directory (listDirectory, canonicalizePath)
import System.FilePath.Posix ((</>))
import Control.Monad

import Data.Maybe
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List
import qualified Data.Set as S

-- This is where all the tests go----------------------

tests :: [TestUnit]
tests = [ TestUnit { className = "T1"
                   , sources = ["java.db"] 
                   , units = [ UField 
                                { field = ("T1.x", "I")
                                , fpurity = FinalStatic
                                , fnullability = NonNullableField
                                }
                            , UMethod 
                                { method = ("T1.f", "(I)I")
                                , mpurity = Pure
                                , mnullability = NonNullableMethod
                                }
                            ]
                   }
        , TestUnit { className = "T2"
                   , sources = ["java.db"]
                   , units = [ UMethod
                                { method = ("T2.fst", "(LPair;)I")
                                , mpurity = Impure
                                , mnullability = NonNullableMethod
                                }
                             , UMethod 
                                { method = ("T2.pr", "()I")
                                , mpurity = Impure
                                , mnullability = NonNullableMethod
                                }
                             ]
                   }
        
        , TestUnit { className = "T3"
                   , sources = ["java.db"]
                   , units = [ UField
                                { field = ("T3.x", "I")
                                , fpurity = Basic
                                , fnullability = NonNullableField
                                }
                             , UMethod 
                                { method = ("T3.f", "(I)V")
                                , mpurity = Local
                                , mnullability = NonNullableMethod
                                }
                             , UMethod 
                                { method = ("T3.g", "()I")
                                , mpurity = Impure
                                , mnullability = NonNullableMethod
                                }
                             , UMethod 
                                { method = ("T3.h", "()LT3;")
                                , mpurity = Pure
                                , mnullability = NullableMethod
                                }
                             , UMethod 
                                { method = ("T3.l", "()LT3;")
                                , mpurity = Pure
                                , mnullability = NullableMethod
                                }
                            ]
                   }
        , TestUnit { className = "T4"
                    , sources = ["java.db"] 
                    , units = [ UMethod 
                                { method = ("T4.f", "()I")
                                , mpurity = Impure
                                , mnullability = NonNullableMethod
                                } 
                             ]
                    }
        , TestUnit { className = "T5"
                    , sources = ["java.db"] 
                    , units = [ UField 
                                 { field = ("T5.x", "I")
                                 , fpurity = Basic
                                 , fnullability = NonNullableField
                                 }
                             , UMethod 
                                 { method = ("T5.f", "()LT5;")
                                 , mpurity = Pure
                                 , mnullability = NonNullableMethod
                                 }
                             ]
                    }
        , TestUnit { className = "T6"
                    , sources = ["java.db"] 
                    , units = [ UField 
                                { field = ("T6.q", "LT6;")
                                , fpurity = Normal
                                , fnullability = NullableField
                                 }
                             , UMethod 
                                { method = ("T6.f", "(I)I")
                                , mpurity = Pure
                                , mnullability = NonNullableMethod
                                }
                            ]
                    }
        ]

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
    , sources :: [FilePath]
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
    absPath <- canonicalizePath testDir
    
    --print absPath

    prov <- classesOnDemandBS absPath

    --print $ prov className

    allDBs <- mapM (\f -> loadAllDB $ absPath </> f) sources 

    let merge = foldl' M.union M.empty
        fDB   = merge $ map afieldDB allDBs
        mDB   = merge $ map amethodDB allDBs
        fDB_n = merge $ map afieldDB_null allDBs
        mDB_n = merge $ map amethodDB_null allDBs
        cls   = S.unions $ map aclasses allDBs
    
    let classes = [className]
        targ = map (\case 
                    UField fld fp fn -> EFieldID fld 
                    UMethod mtd mp mn -> EMethodID mtd
                ) units
        
        ainput = AnalysisInputType 
                { classes = classes
                , targets = targ
                , classProvider = prov
                , sourceClasses = cls
                , sourceFieldDB = fDB
                , sourceMethodDB = mDB
                , sourceFieldNullabilityDB = fDB_n
                , sourceMethodNullabilityDB = mDB_n
                }

        result = analysis ainput
        
        emsg = if isLeft result 
                then "Classes not found : " ++ intercalate "\n    " (map T.unpack $ fromLeft undefined result)
                else ""


        output = fromRight (error emsg) result

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

