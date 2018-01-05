module Semantic.Properties.OnInput where

import Semantic.Properties.Utils

class HasOnInputProp a where
    type OnInputProp a
    getOnInput :: a -> OnInputProp a 
    setOnInput :: OnInputProp a -> a -> a

pattern OnInput :: HasOnInputProp a => OnInputProp a -> a -> a
pattern OnInput f a <- (getOnInput &&& id -> (f,a)) where
    OnInput f a = setOnInput f a