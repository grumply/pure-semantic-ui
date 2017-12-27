module Semantic.Properties.Internal where

import Semantic.Properties.Utils

class HasInternalProp a where
    getInternal :: a -> Bool
    setInternal :: Bool -> a -> a

pattern Internal :: HasInternalProp a => a -> a
pattern Internal a <- (getInternal &&& id -> (True,a)) where
    Internal a = setInternal True a