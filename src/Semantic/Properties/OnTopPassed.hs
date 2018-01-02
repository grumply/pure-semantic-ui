module Semantic.Properties.OnTopPassed where

import Semantic.Properties.Utils

class HasOnTopPassedProp a where
    type OnTopPassedProp a
    getOnTopPassed :: a -> OnTopPassedProp a
    setOnTopPassed :: OnTopPassedProp a -> a -> a

pattern OnTopPassed :: HasOnTopPassedProp a => OnTopPassedProp a -> a -> a
pattern OnTopPassed ou a <- (getOnTopPassed &&& id -> (ou,a)) where
    OnTopPassed ou a = setOnTopPassed ou a