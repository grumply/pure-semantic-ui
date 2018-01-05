module Semantic.Properties.Open where

import Semantic.Properties.Utils

class HasOpenProp a where
    type OpenProp a :: *
    type OpenProp a = Bool
    getOpen :: a -> OpenProp a
    setOpen :: OpenProp a -> a -> a

pattern Open :: HasOpenProp a => OpenProp a -> a -> a
pattern Open o a <- (getOpen &&& id -> (o,a)) where
    Open o a = setOpen o a