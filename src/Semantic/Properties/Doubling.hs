module Semantic.Properties.Doubling where

import Semantic.Properties.Utils

class HasDoublingProp a where
    getDoubling :: a -> Bool
    setDoubling :: Bool -> a -> a

pattern Doubling :: HasDoublingProp a => a -> a
pattern Doubling a <- (getDoubling &&& id -> (True,a)) where
    Doubling a = setDoubling True a