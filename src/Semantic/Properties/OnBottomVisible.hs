module Semantic.Properties.OnBottomVisible where

import Semantic.Properties.Utils

class HasOnBottomVisibleProp a where
    type OnBottomVisibleProp a
    getOnBottomVisible :: a -> OnBottomVisibleProp a
    setOnBottomVisible :: OnBottomVisibleProp a -> a -> a

pattern OnBottomVisible :: HasOnBottomVisibleProp a => OnBottomVisibleProp a -> a -> a
pattern OnBottomVisible ou a <- (getOnBottomVisible &&& id -> (ou,a)) where
    OnBottomVisible ou a = setOnBottomVisible ou a