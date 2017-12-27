module Semantic.Properties.Primary where

import Semantic.Properties.Utils

class HasPrimaryProp a where
    getPrimary :: a -> Bool
    setPrimary :: Bool -> a -> a

pattern Primary :: HasPrimaryProp a => a -> a
pattern Primary a <- (getPrimary &&& id -> (True,a)) where
    Primary a = setPrimary True a