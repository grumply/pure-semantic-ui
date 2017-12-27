module Semantic.Properties.Position where

import Semantic.Properties.Utils

class HasPositionProp a where
    getPosition :: a -> Txt
    setPosition :: Txt -> a -> a

pattern Position :: HasPositionProp a => Txt -> a -> a
pattern Position p a <- (getPosition &&& id -> (p,a)) where
    Position p a = setPosition p a