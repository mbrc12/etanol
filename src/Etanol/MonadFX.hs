{- 
        MonadFX : This library provides the FX monad (Fast eXit) which has
                  state capabilities along with fast exiting. It is known
                  that it is trivial to implement this monad using monad
                  transformers alone, with some synonyms of functions, but
                  the author wrote this to avoid complicated types, so that
                  when more complicated routines are written using this, he
                  does not get confused in the monadic part alone, which is
                  introduced just for the sake of convenience.

        Author : Mriganka Basu Roy Chowdhury, 28th May, 2018
-}

module Etanol.MonadFX (
                FX(..), 
                getS, putS,
                exitWith, resultant,
                whenExit, whenNotExit,
                whenDoExit,
                exceptify
        ) where

import Control.Monad
import Data.Either
import Data.Functor
import Control.Applicative

newtype FX s r x = FX { runFX :: s -> (s, Either r x) }

unLeft :: Either a b -> a
unLeft (Left x) = x
unLeft _        = error "unLeft called on non left Either a b."

unRight :: Either a b -> b
unRight (Right x) = x
unRight _         = error "unRight called on non right Either a b."

getS :: FX s r s
getS = FX $ \s -> (s, Right s)

putS :: s -> FX s r ()
putS s = FX $ \_ -> (s, Right ())

exitWith :: r -> FX s r ()               -- exits with r
exitWith r = FX $ \s -> (s, Left r)

resultant :: FX s r r -> s -> (s, r)
resultant m s = let (t, z) = runFX m s 
                in  if isLeft z then (t, unLeft z) else (t, unRight z)

-- a beautiful function that does not allow the inner early exit to make your function exit as well
exceptify :: FX s r r -> FX s r r
exceptify m = FX $ \s -> let (t, q) = runFX m s 
                         in  if isLeft q 
                             then (t, Right $ unLeft q)
                             else (t, q)
                              

whenExit :: Bool -> r -> FX s r ()
whenExit cond r = if cond then exitWith r else return ()

whenNotExit :: Bool -> r -> FX s r ()
whenNotExit cond = whenExit (not cond)

whenDoExit :: Bool -> FX s r () -> r -> FX s r ()
whenDoExit cond work r =        if cond 
                                then work >> whenExit True r 
                                else return ()
                                        

instance Monad (FX s r) where
        return z = FX $ \s -> (s, Right z)
       
        m >>= g  = FX $ \s ->   let (t, z) = runFX m s 
                                in  if isLeft z
                                    then (t, Left $ unLeft z)
                                    else let p = unRight z 
                                         in runFX (g p) t 

instance Functor (FX s r) where
        fmap f xs = xs >>= return . f 

instance Applicative (FX s r) where
        pure      = return
        fs <*> xs = fs >>= \f -> xs >>= return . f
