module Semantic.Properties.Instant where

import Semantic.Properties.Utils

class HasInstantProp a where
    getInstant :: a -> Bool
    setInstant :: Bool -> a -> a

pattern Instant :: HasInstantProp a => a -> a
pattern Instant a <- (getInstant &&& id -> (True,a)) where
    Instant a = setInstant True a