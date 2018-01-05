module Semantic.Properties.Rows where

import Semantic.Properties.Utils

class HasRowsProp a where
    getRows :: a -> Int
    setRows :: Int -> a -> a

pattern Rows :: HasRowsProp a => Int -> a -> a
pattern Rows n a <- (getRows &&& id -> (n,a)) where
    Rows n a = setRows n a