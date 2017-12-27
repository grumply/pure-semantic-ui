module Semantic.Properties.Compact where

import Semantic.Properties.Utils

class HasCompactProp a where
    getCompact :: a -> Bool
    setCompact :: Bool -> a -> a

pattern Compact :: HasCompactProp a => a -> a
pattern Compact a <- (getCompact &&& id -> (True,a)) where
    Compact a = setCompact True a