module Semantic.Properties.OnPassing where

import Semantic.Properties.Utils

class HasOnPassingProp a where
    type OnPassingProp a
    getOnPassing :: a -> OnPassingProp a
    setOnPassing :: OnPassingProp a -> a -> a

pattern OnPassing :: HasOnPassingProp a => OnPassingProp a -> a -> a
pattern OnPassing ou a <- (getOnPassing &&& id -> (ou,a)) where
    OnPassing ou a = setOnPassing ou a