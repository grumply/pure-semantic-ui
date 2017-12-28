module Semantic.Properties.Stackable where

import Semantic.Properties.Utils

class HasStackableProp a where
    type StackableProp a :: *
    type StackableProp a = Txt
    getStackable :: a -> StackableProp a
    setStackable :: StackableProp a -> a -> a

pattern Stackable :: HasStackableProp a => StackableProp a -> a -> a
pattern Stackable s a <- (getStackable &&& id -> (s,a)) where
    Stackable s a = setStackable s a