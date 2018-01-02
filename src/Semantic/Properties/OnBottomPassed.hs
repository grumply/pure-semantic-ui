module Semantic.Properties.OnBottomPassed where

import Semantic.Properties.Utils

class HasOnBottomPassedProp a where
    type OnBottomPassedProp a
    getOnBottomPassed :: a -> OnBottomPassedProp a
    setOnBottomPassed :: OnBottomPassedProp a -> a -> a

pattern OnBottomPassed :: HasOnBottomPassedProp a => OnBottomPassedProp a -> a -> a
pattern OnBottomPassed ou a <- (getOnBottomPassed &&& id -> (ou,a)) where
    OnBottomPassed ou a = setOnBottomPassed ou a