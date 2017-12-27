module Semantic.Properties.Flipped where

import Semantic.Properties.Utils

class HasFlippedProp a where
    getFlipped :: a -> Txt
    setFlipped :: Txt -> a -> a

pattern Flipped :: HasFlippedProp a => Txt -> a -> a
pattern Flipped f a <- (getFlipped &&& id -> (f,a)) where
    Flipped f a = setFlipped f a
