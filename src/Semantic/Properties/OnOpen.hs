module Semantic.Properties.OnOpen where

import Semantic.Properties.Utils

class HasOnOpenProp a where
    type OnOpenProp a
    getOnOpen :: a -> OnOpenProp a
    setOnOpen :: OnOpenProp a -> a -> a

pattern OnOpen :: HasOnOpenProp a => OnOpenProp a -> a -> a
pattern OnOpen oc a <- (getOnOpen &&& id -> (oc,a)) where
    OnOpen oc a = setOnOpen oc a