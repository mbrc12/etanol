module Etanol.Utils 
    ( classDeps
    ) where

import ByteCodeParser.BasicTypes
import Etanol.Decompile

import qualified Data.Text as T


toClassName :: T.Text -> ClassName  -- remove trailing stuff
toClassName s = T.reverse $ T.tail $ T.dropWhile (/= '.') $ T.reverse s

classDeps :: [ConstantInfo] -> [ClassName]  -- one step class dependencies
classDeps = undefined
{--
classDeps cpool = uniquesOf $! (map (toClassName . getFieldName cpool) $
                                    filter (\elem -> 
                                            constType 
                                             (cpool !! elem) == CFieldRef) 
                                        [0..]) ++
                               (map (toClassName . getMethodName cpool) $
                                    filter (\elem ->
                                            constType
                                             (cpool !! elem) 
--}                                    
