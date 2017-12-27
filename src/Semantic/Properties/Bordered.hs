module Semantic.Properties.Bordered where

import Semantic.Properties.Utils

class HasBorderedProp a where
    getBordered :: a -> Bool
    setBordered :: Bool -> a -> a

pattern Bordered :: HasBorderedProp a => a -> a
pattern Bordered a <- (getBordered &&& id -> (True,a)) where
    Bordered a = setBordered True a