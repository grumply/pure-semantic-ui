module Semantic.Extensions.Basic where

import Semantic.Extensions.Utils

class HasBasic a where
    getBasic :: a -> Bool
    setBasic :: Bool -> a -> a

pattern Basic :: HasBasic a => a -> a
pattern Basic a <- (getBasic &&& id -> (True,a)) where
    Basic a = setBasic True a