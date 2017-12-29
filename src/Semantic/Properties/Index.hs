module Semantic.Properties.Index where

import Semantic.Properties.Utils

class HasIndexProp a where
    getIndex :: a -> Int
    setIndex :: Int -> a -> a

pattern Index :: HasIndexProp a => Int -> a -> a
pattern Index i a <- (getIndex &&& id -> (i,a)) where
    Index i a = setIndex i a