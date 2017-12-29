module Semantic.Properties.Fixed where

import Semantic.Properties.Utils

class HasFixedProp a where
    type FixedProp a :: *
    type FixedProp a = Txt
    getFixed :: a -> FixedProp a
    setFixed :: FixedProp a -> a -> a

pattern Fixed :: HasFixedProp a => FixedProp a -> a -> a
pattern Fixed f a <- (getFixed &&& id -> (f,a)) where
    Fixed f a = setFixed f a