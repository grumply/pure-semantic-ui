module Semantic.Properties.OnClick where

import Semantic.Properties.Utils

class HasOnClickProp a where
    type OnClickProp a
    getOnClick :: a -> OnClickProp a
    setOnClick :: OnClickProp a -> a -> a

pattern OnClick :: HasOnClickProp a => OnClickProp a -> a -> a
pattern OnClick f a <- (getOnClick &&& id -> (f,a)) where
    OnClick f a = setOnClick f a