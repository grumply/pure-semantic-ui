module Semantic.Properties.InnerRef where

import Semantic.Properties.Utils

class HasInnerRefProp a where
    type InnerRefProp a
    getInnerRef :: a -> InnerRefProp a
    setInnerRef :: InnerRefProp a -> a -> a

pattern InnerRef :: HasInnerRefProp a => InnerRefProp a -> a -> a
pattern InnerRef ir a <- (getInnerRef &&& id -> (ir,a)) where
    InnerRef ir a = setInnerRef ir a