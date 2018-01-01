module Semantic.Properties.OnUnmount where

import Semantic.Properties.Utils

class HasOnUnmountProp a where
    type OnUnmountProp a
    getOnUnmount :: a -> OnUnmountProp a
    setOnUnmount :: OnUnmountProp a -> a -> a

pattern OnUnmount :: HasOnUnmountProp a => OnUnmountProp a -> a -> a
pattern OnUnmount oc a <- (getOnUnmount &&& id -> (oc,a)) where
    OnUnmount oc a = setOnUnmount oc a