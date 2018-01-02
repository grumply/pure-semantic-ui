module Semantic.Properties.OnBottomVisibleReverse where

import Semantic.Properties.Utils

class HasOnBottomVisibleReverseProp a where
    type OnBottomVisibleReverseProp a
    getOnBottomVisibleReverse :: a -> OnBottomVisibleReverseProp a
    setOnBottomVisibleReverse :: OnBottomVisibleReverseProp a -> a -> a

pattern OnBottomVisibleReverse :: HasOnBottomVisibleReverseProp a => OnBottomVisibleReverseProp a -> a -> a
pattern OnBottomVisibleReverse ou a <- (getOnBottomVisibleReverse &&& id -> (ou,a)) where
    OnBottomVisibleReverse ou a = setOnBottomVisibleReverse ou a