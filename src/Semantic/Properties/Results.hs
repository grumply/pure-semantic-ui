module Semantic.Properties.Results where

import Semantic.Properties.Utils

class HasResultsProp a where
    type ResultsProp a
    getResults :: a -> ResultsProp a
    setResults :: ResultsProp a -> a -> a

pattern Results :: HasResultsProp a => ResultsProp a -> a -> a
pattern Results rs a <- (getResults &&& id -> (rs,a)) where
    Results rs a = setResults rs a