module Semantic.Properties.OnShow where

import Semantic.Properties.Utils

class HasOnShowProp a where
    type OnShowProp a
    getOnShow :: a -> OnShowProp a
    setOnShow :: OnShowProp a -> a -> a

pattern OnShow :: HasOnShowProp a => OnShowProp a -> a -> a
pattern OnShow os a <- (getOnShow &&& id -> (os,a)) where
    OnShow os a = setOnShow os a