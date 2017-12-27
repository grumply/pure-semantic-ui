module Semantic.Properties.Positive where

import Semantic.Properties.Utils

class HasPositiveProp a where
    getPositive :: a -> Bool
    setPositive :: Bool -> a -> a

pattern Positive :: HasPositiveProp a => a -> a
pattern Positive a <- (getPositive &&& id -> (True,a)) where
    Positive a = setPositive True a