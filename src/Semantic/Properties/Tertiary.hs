module Semantic.Properties.Tertiary where

import Semantic.Properties.Utils

class HasTertiaryProp a where
    getTertiary :: a -> Bool
    setTertiary :: Bool -> a -> a

pattern Tertiary :: HasTertiaryProp a => a -> a
pattern Tertiary a <- (getTertiary &&& id -> (True,a)) where
    Tertiary a = setTertiary True a