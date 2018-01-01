module Semantic.Properties.OnMount where

import Semantic.Properties.Utils

class HasOnMountProp a where
    type OnMountProp a
    getOnMount :: a -> OnMountProp a
    setOnMount :: OnMountProp a -> a -> a

pattern OnMount :: HasOnMountProp a => OnMountProp a -> a -> a
pattern OnMount oc a <- (getOnMount &&& id -> (oc,a)) where
    OnMount oc a = setOnMount oc a