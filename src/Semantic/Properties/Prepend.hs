module Semantic.Properties.Prepend where

import Semantic.Properties.Utils

class HasPrependProp a where
    getPrepend :: a -> Bool
    setPrepend :: Bool -> a -> a

pattern Prepend :: HasPrependProp a => Bool -> a -> a
pattern Prepend o a <- (getPrepend &&& id -> (o,a)) where
    Prepend o a = setPrepend o a