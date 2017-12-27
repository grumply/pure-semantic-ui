module Semantic.Properties.Horizontal where

import Semantic.Properties.Utils

class HasHorizontalProp a where
    getHorizontal :: a -> Bool
    setHorizontal :: Bool -> a -> a

pattern Horizontal :: HasHorizontalProp a => a -> a
pattern Horizontal a <- (getHorizontal &&& id -> (True,a)) where
    Horizontal a = setHorizontal True a