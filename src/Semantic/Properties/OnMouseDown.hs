module Semantic.Properties.OnMouseDown where

import Semantic.Properties.Utils

class HasOnMouseDownProp a where
    type OnMouseDownProp a
    getOnMouseDown :: a -> OnMouseDownProp a
    setOnMouseDown :: OnMouseDownProp a -> a -> a

pattern OnMouseDown :: HasOnMouseDownProp a => OnMouseDownProp a -> a -> a
pattern OnMouseDown omd a <- (getOnMouseDown &&& id -> (omd,a)) where
    OnMouseDown omd a = setOnMouseDown omd a