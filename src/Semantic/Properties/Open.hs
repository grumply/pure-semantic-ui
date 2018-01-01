module Semantic.Properties.Open where

import Semantic.Properties.Utils

class HasOpenProp a where
    getOpen :: a -> Bool
    setOpen :: Bool -> a -> a

pattern Open :: HasOpenProp a => Bool -> a -> a
pattern Open o a <- (getOpen &&& id -> (o,a)) where
    Open o a = setOpen o a