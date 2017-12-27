module Semantic.Properties.Rotated where

import Semantic.Properties.Utils

class HasRotatedProp a where
    getRotated :: a -> Txt
    setRotated :: Txt -> a -> a

pattern Rotated :: HasRotatedProp a => Txt -> a -> a
pattern Rotated r a <- (getRotated &&& id -> (r,a)) where
    Rotated r a = setRotated r a

