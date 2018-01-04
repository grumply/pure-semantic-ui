module Semantic.Properties.Total where

import Semantic.Properties.Utils

class HasTotalProp a where
    getTotal :: a -> Int
    setTotal :: Int -> a -> a

pattern Total :: HasTotalProp a => Int -> a -> a
pattern Total p a <- (getTotal &&& id -> (p,a)) where
    Total p a = setTotal p a