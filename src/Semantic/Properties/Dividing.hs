module Semantic.Properties.Dividing where

import Semantic.Properties.Utils

class HasDividingProp a where
    getDividing :: a -> Bool
    setDividing :: Bool -> a -> a

pattern Dividing :: HasDividingProp a => a -> a
pattern Dividing a <- (getDividing &&& id -> (True,a)) where
    Dividing a = setDividing True a