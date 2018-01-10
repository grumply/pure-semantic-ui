module Semantic.Properties.OnBlur where

import Semantic.Properties.Utils

class HasOnBlurProp a where
    type OnBlurProp a
    getOnBlur :: a -> OnBlurProp a
    setOnBlur :: OnBlurProp a -> a -> a

pattern OnBlur :: HasOnBlurProp a => OnBlurProp a -> a -> a
pattern OnBlur ob a <- (getOnBlur &&& id -> (ob,a)) where
    OnBlur ob a = setOnBlur ob a