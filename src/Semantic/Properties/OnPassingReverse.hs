module Semantic.Properties.OnPassingReverse where

import Semantic.Properties.Utils

class HasOnPassingReverseProp a where
    type OnPassingReverseProp a
    getOnPassingReverse :: a -> OnPassingReverseProp a
    setOnPassingReverse :: OnPassingReverseProp a -> a -> a

pattern OnPassingReverse :: HasOnPassingReverseProp a => OnPassingReverseProp a -> a -> a
pattern OnPassingReverse ou a <- (getOnPassingReverse &&& id -> (ou,a)) where
    OnPassingReverse ou a = setOnPassingReverse ou a