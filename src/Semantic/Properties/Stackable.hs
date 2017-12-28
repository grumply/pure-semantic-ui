module Semantic.Properties.Stackable where

import Semantic.Properties.Utils

class HasStackableProp a where
    getStackable :: a -> Txt
    setStackable :: Txt -> a -> a

pattern Stackable :: HasStackableProp a => Txt -> a -> a
pattern Stackable s a <- (getStackable &&& id -> (s,a)) where
    Stackable s a = setStackable s a