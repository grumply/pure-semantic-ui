module Semantic.Properties.Empty where

import Semantic.Properties.Utils

class HasEmptyProp a where
    getEmpty :: a -> Bool
    setEmpty :: Bool -> a -> a

pattern Empty :: HasEmptyProp a => a -> a
pattern Empty a <- (getEmpty &&& id -> (True,a)) where
    Empty a = setEmpty True a