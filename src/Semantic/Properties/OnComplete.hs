module Semantic.Properties.OnComplete where

import Semantic.Properties.Utils

class HasOnCompleteProp a where
    type OnCompleteProp a
    getOnComplete :: a -> OnCompleteProp a
    setOnComplete :: OnCompleteProp a -> a -> a

pattern OnComplete :: HasOnCompleteProp a => OnCompleteProp a -> a -> a
pattern OnComplete oc a <- (getOnComplete &&& id -> (oc,a)) where
    OnComplete oc a = setOnComplete oc a