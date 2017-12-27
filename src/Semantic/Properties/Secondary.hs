module Semantic.Properties.Secondary where

import Semantic.Properties.Utils

class HasSecondaryProp a where
    getSecondary :: a -> Bool
    setSecondary :: Bool -> a -> a

pattern Secondary :: HasSecondaryProp a => a -> a
pattern Secondary a <- (getSecondary &&& id -> (True,a)) where
    Secondary a = setSecondary True a