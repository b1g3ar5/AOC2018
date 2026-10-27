--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}

module Recursion where

import Control.Arrow --( (<<<), (>>>) )
import Data.Function ( (&) )


newtype Fix f = Fix { unFix :: f (Fix f) } 

type Algebra f a = f a -> a
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata f = unFix >>> fmap (cata f) >>> f
type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana f = Fix <<< fmap (ana f) <<< f


data TreeF a r = NilF | TreeF a [r] deriving (Show, Functor)
type Tree a = Fix (TreeF a)

