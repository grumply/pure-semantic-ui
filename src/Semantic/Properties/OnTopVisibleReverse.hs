module Semantic.Properties.OnTopVisibleReverse where

import Semantic.Properties.Utils

class HasOnTopVisibleReverseProp a where
    type OnTopVisibleReverseProp a
    getOnTopVisibleReverse :: a -> OnTopVisibleReverseProp a
    setOnTopVisibleReverse :: OnTopVisibleReverseProp a -> a -> a

pattern OnTopVisibleReverse :: HasOnTopVisibleReverseProp a => OnTopVisibleReverseProp a -> a -> a
pattern OnTopVisibleReverse ou a <- (getOnTopVisibleReverse &&& id -> (ou,a)) where
    OnTopVisibleReverse ou a = setOnTopVisibleReverse ou a