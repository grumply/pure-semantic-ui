module Semantic.Properties.Minimal where

import Semantic.Properties.Utils

class HasMinimalProp a where
    getMinimal :: a -> Bool
    setMinimal :: Bool -> a -> a

pattern Minimal :: HasMinimalProp a => a -> a
pattern Minimal a <- (getMinimal &&& id -> (True,a)) where
    Minimal a = setMinimal True a