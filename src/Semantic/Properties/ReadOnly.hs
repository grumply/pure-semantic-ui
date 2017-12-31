module Semantic.Properties.ReadOnly where

import Semantic.Properties.Utils

class HasReadOnlyProp a where
    getReadOnly :: a -> Bool
    setReadOnly :: Bool -> a -> a

pattern ReadOnly :: HasReadOnlyProp a => a -> a
pattern ReadOnly a <- (getReadOnly &&& id -> (True,a)) where
    ReadOnly a = setReadOnly True a