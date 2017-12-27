module Semantic.Properties.Circular where

import Semantic.Properties.Utils

class HasCircularProp a where
    getCircular :: a -> Bool
    setCircular :: Bool -> a -> a

pattern Circular :: HasCircularProp a => a -> a
pattern Circular a <- (getCircular &&& id -> (True,a)) where
    Circular a = setCircular True a