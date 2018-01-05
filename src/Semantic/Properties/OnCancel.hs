module Semantic.Properties.OnCancel where

import Semantic.Properties.Utils

class HasOnCancelProp a where
    type OnCancelProp a
    getOnCancel :: a -> OnCancelProp a
    setOnCancel :: OnCancelProp a -> a -> a

pattern OnCancel :: HasOnCancelProp a => OnCancelProp a -> a -> a
pattern OnCancel oc a <- (getOnCancel &&& id -> (oc,a)) where
    OnCancel oc a = setOnCancel oc a