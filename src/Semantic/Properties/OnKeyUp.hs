module Semantic.Properties.OnKeyUp where

import Semantic.Properties.Utils

class HasOnKeyUpProp a where
    type OnKeyUpProp a
    getOnKeyUp :: a -> OnKeyUpProp a 
    setOnKeyUp :: OnKeyUpProp a -> a -> a

pattern OnKeyUp :: HasOnKeyUpProp a => OnKeyUpProp a -> a -> a
pattern OnKeyUp f a <- (getOnKeyUp &&& id -> (f,a)) where
    OnKeyUp f a = setOnKeyUp f a