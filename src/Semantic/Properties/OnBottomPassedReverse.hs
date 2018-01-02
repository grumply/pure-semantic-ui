module Semantic.Properties.OnBottomPassedReverse where

import Semantic.Properties.Utils

class HasOnBottomPassedReverseProp a where
    type OnBottomPassedReverseProp a
    getOnBottomPassedReverse :: a -> OnBottomPassedReverseProp a
    setOnBottomPassedReverse :: OnBottomPassedReverseProp a -> a -> a

pattern OnBottomPassedReverse :: HasOnBottomPassedReverseProp a => OnBottomPassedReverseProp a -> a -> a
pattern OnBottomPassedReverse ou a <- (getOnBottomPassedReverse &&& id -> (ou,a)) where
    OnBottomPassedReverse ou a = setOnBottomPassedReverse ou a