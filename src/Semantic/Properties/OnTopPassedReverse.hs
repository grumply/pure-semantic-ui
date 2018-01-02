module Semantic.Properties.OnTopPassedReverse where

import Semantic.Properties.Utils

class HasOnTopPassedReverseProp a where
    type OnTopPassedReverseProp a
    getOnTopPassedReverse :: a -> OnTopPassedReverseProp a
    setOnTopPassedReverse :: OnTopPassedReverseProp a -> a -> a

pattern OnTopPassedReverse :: HasOnTopPassedReverseProp a => OnTopPassedReverseProp a -> a -> a
pattern OnTopPassedReverse ou a <- (getOnTopPassedReverse &&& id -> (ou,a)) where
    OnTopPassedReverse ou a = setOnTopPassedReverse ou a