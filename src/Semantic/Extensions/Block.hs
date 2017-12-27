module Semantic.Extensions.Block where

import Semantic.Extensions.Utils

class HasBlock a where
    getBlock :: a -> Bool
    setBlock :: Bool -> a -> a

pattern Block :: HasBlock a => a -> a
pattern Block a <- (getBlock &&& id -> (True,a)) where
    Block a = setBlock True a