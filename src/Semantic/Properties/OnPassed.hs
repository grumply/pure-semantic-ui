module Semantic.Properties.OnPassed where

import Semantic.Properties.Utils

class HasOnPassedProp a where
    type OnPassedProp a
    getOnPassed :: a -> OnPassedProp a
    setOnPassed :: OnPassedProp a -> a -> a

pattern OnPassed :: HasOnPassedProp a => OnPassedProp a -> a -> a
pattern OnPassed ou a <- (getOnPassed &&& id -> (ou,a)) where
    OnPassed ou a = setOnPassed ou a