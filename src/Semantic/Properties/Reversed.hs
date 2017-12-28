module Semantic.Properties.Reversed where

import Semantic.Properties.Utils

class HasReversedProp a where
    type ReversedProp a
    getReversed :: a -> ReversedProp a 
    setReversed :: ReversedProp a -> a -> a

pattern Reversed :: HasReversedProp a => ReversedProp a -> a -> a
pattern Reversed r a <- (getReversed &&& id -> (r,a)) where
    Reversed r a = setReversed r a