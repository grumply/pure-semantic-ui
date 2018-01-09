module Semantic.Properties.OnBottom where

import Semantic.Properties.Utils

class HasOnBottomProp a where
    type OnBottomProp a
    getOnBottom :: a -> OnBottomProp a
    setOnBottom :: OnBottomProp a -> a -> a

pattern OnBottom :: HasOnBottomProp a => OnBottomProp a -> a -> a
pattern OnBottom obp a <- (getOnBottom &&& id -> (obp,a)) where
    OnBottom obp a = setOnBottom obp a
