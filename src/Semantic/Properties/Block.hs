module Semantic.Properties.Block where

import Semantic.Properties.Utils

class HasBlockProp a where
    getBlock :: a -> Bool
    setBlock :: Bool -> a -> a

pattern Block :: HasBlockProp a => a -> a
pattern Block a <- (getBlock &&& id -> (True,a)) where
    Block a = setBlock True a