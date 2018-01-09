module Semantic.Properties.OnTop where

import Semantic.Properties.Utils

class HasOnTopProp a where
    type OnTopProp a
    getOnTop :: a -> OnTopProp a
    setOnTop :: OnTopProp a -> a -> a

pattern OnTop :: HasOnTopProp a => OnTopProp a -> a -> a
pattern OnTop ou a <- (getOnTop &&& id -> (ou,a)) where
    OnTop ou a = setOnTop ou a