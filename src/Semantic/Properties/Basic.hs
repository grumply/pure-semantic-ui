module Semantic.Properties.Basic where

import Semantic.Properties.Utils

class HasBasicProp a where
    getBasic :: a -> Bool
    setBasic :: Bool -> a -> a

pattern Basic :: HasBasicProp a => a -> a
pattern Basic a <- (getBasic &&& id -> (True,a)) where
    Basic a = setBasic True a