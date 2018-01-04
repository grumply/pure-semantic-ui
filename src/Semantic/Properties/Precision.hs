module Semantic.Properties.Precision where

import Semantic.Properties.Utils

class HasPrecisionProp a where
    getPrecision :: a -> Int
    setPrecision :: Int -> a -> a

pattern Precision :: HasPrecisionProp a => Int -> a -> a
pattern Precision p a <- (getPrecision &&& id -> (p,a)) where
    Precision p a = setPrecision p a