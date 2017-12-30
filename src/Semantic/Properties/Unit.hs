module Semantic.Properties.Unit where

import Semantic.Properties.Utils

class HasUnitProp a where
    getUnit :: a -> Txt
    setUnit :: Txt -> a -> a

pattern Unit :: HasUnitProp a => Txt -> a -> a
pattern Unit u a <- (getUnit &&& id -> (u,a)) where
    Unit u a = setUnit u a