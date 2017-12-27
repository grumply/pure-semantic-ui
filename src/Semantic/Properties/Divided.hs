module Semantic.Properties.Divided where

import Semantic.Properties.Utils

class HasDividedProp a where
    getDivided :: a -> Bool
    setDivided :: Bool -> a -> a

pattern Divided :: HasDividedProp a => a -> a
pattern Divided a <- (getDivided &&& id -> (True,a)) where
    Divided a = setDivided True a