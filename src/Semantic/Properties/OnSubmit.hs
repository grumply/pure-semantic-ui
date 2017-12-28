module Semantic.Properties.OnSubmit where

import Semantic.Properties.Utils

class HasOnSubmitProp a where
    type OnSubmitProp a
    getOnSubmit :: a -> OnSubmitProp a
    setOnSubmit :: OnSubmitProp a -> a -> a

pattern OnSubmit :: HasOnSubmitProp a => OnSubmitProp a -> a -> a
pattern OnSubmit f a <- (getOnSubmit &&& id -> (f,a)) where
    OnSubmit f a = setOnSubmit f a