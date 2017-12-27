module Semantic.Properties.Ordered where

import Semantic.Properties.Utils

class HasOrderedProp a where
    getOrdered :: a -> Bool
    setOrdered :: Bool -> a -> a

pattern Ordered :: HasOrderedProp a => a -> a
pattern Ordered a <- (getOrdered &&& id -> (True,a)) where
    Ordered a = setOrdered True a