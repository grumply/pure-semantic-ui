module Semantic.Properties.OnConfirm where

import Semantic.Properties.Utils

class HasOnConfirmProp a where
    type OnConfirmProp a
    getOnConfirm :: a -> OnConfirmProp a
    setOnConfirm :: OnConfirmProp a -> a -> a

pattern OnConfirm :: HasOnConfirmProp a => OnConfirmProp a -> a -> a
pattern OnConfirm oc a <- (getOnConfirm &&& id -> (oc,a)) where
    OnConfirm oc a = setOnConfirm oc a