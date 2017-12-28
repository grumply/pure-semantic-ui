module Semantic.Properties.Stretched where

import Semantic.Properties.Utils

class HasStretchedProp a where
    getStretched :: a -> Bool
    setStretched :: Bool -> a -> a

pattern Stretched :: HasStretchedProp a => a -> a
pattern Stretched a <- (getStretched &&& id -> (True,a)) where
    Stretched a = setStretched True a