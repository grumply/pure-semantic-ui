module Semantic.Properties.Transparent where

import Semantic.Properties.Utils

class HasTransparentProp a where
    getTransparent :: a -> Bool
    setTransparent :: Bool -> a -> a

pattern Transparent :: HasTransparentProp a => a -> a
pattern Transparent a <- (getTransparent &&& id -> (True,a)) where
    Transparent a = setTransparent True a