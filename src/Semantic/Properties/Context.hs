module Semantic.Properties.Context where

import Semantic.Properties.Utils

class HasContextProp a where
    type ContextProp a
    getContext :: a -> ContextProp a
    setContext :: ContextProp a -> a -> a

pattern Context :: HasContextProp a => ContextProp a -> a -> a
pattern Context c a <- (getContext &&& id -> (c,a)) where
    Context c a = setContext c a