module Semantic.Properties.Fixed where

import Semantic.Properties.Utils

class HasFixedProp a where
    getFixed :: a -> Txt
    setFixed :: Txt -> a -> a

pattern Fixed :: HasFixedProp a => Txt -> a -> a
pattern Fixed f a <- (getFixed &&& id -> (f,a)) where
    Fixed f a = setFixed f a