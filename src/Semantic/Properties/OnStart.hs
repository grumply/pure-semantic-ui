module Semantic.Properties.OnStart where

import Semantic.Properties.Utils

class HasOnStartProp a where
    type OnStartProp a
    getOnStart :: a -> OnStartProp a
    setOnStart :: OnStartProp a -> a -> a

pattern OnStart :: HasOnStartProp a => OnStartProp a -> a -> a
pattern OnStart os a <- (getOnStart &&& id -> (os,a)) where
    OnStart os a = setOnStart os a