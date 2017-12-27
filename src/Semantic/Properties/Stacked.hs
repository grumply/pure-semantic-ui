module Semantic.Properties.Stacked where

import Semantic.Properties.Utils

class HasStackedProp a where
    getStacked :: a -> Bool
    setStacked :: Bool -> a -> a

pattern Stacked :: HasStackedProp a => a -> a
pattern Stacked a <- (getStacked &&& id -> (True,a)) where
    Stacked a = setStacked True a