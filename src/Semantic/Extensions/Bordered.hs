module Semantic.Extensions.Bordered where

import Semantic.Extensions.Utils

class HasBordered a where
    getBordered :: a -> Bool
    setBordered :: Bool -> a -> a

pattern Bordered :: HasBordered a => a -> a
pattern Bordered a <- (getBordered &&& id -> (True,a)) where
    Bordered a = setBordered True a