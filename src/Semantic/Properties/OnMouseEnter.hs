module Semantic.Properties.OnMouseEnter where

import Semantic.Properties.Utils

class HasOnMouseEnterProp a where
    type OnMouseEnterProp a
    getOnMouseEnter :: a -> OnMouseEnterProp a 
    setOnMouseEnter :: OnMouseEnterProp a -> a -> a

pattern OnMouseEnter :: HasOnMouseEnterProp a => OnMouseEnterProp a -> a -> a
pattern OnMouseEnter f a <- (getOnMouseEnter &&& id -> (f,a)) where
    OnMouseEnter f a = setOnMouseEnter f a