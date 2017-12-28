module Semantic.Properties.Divided where

import Semantic.Properties.Utils

class HasDividedProp a where
    type DividedProp a :: *
    type DividedProp a = Bool
    getDivided :: a -> DividedProp a
    setDivided :: DividedProp a -> a -> a

pattern Divided :: HasDividedProp a => DividedProp a -> a -> a
pattern Divided d a <- (getDivided &&& id -> (d,a)) where
    Divided d a = setDivided d a