module Semantic.Properties.OnTopVisible where

import Semantic.Properties.Utils

class HasOnTopVisibleProp a where
    type OnTopVisibleProp a
    getOnTopVisible :: a -> OnTopVisibleProp a
    setOnTopVisible :: OnTopVisibleProp a -> a -> a

pattern OnTopVisible :: HasOnTopVisibleProp a => OnTopVisibleProp a -> a -> a
pattern OnTopVisible ou a <- (getOnTopVisible &&& id -> (ou,a)) where
    OnTopVisible ou a = setOnTopVisible ou a