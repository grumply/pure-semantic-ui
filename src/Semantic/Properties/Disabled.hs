module Semantic.Properties.Disabled where

import Semantic.Properties.Utils

class HasDisabledProp a where
    getDisabled :: a -> Bool
    setDisabled :: Bool -> a -> a

pattern Disabled :: HasDisabledProp a => Bool -> a -> a
pattern Disabled b a <- (getDisabled &&& id -> (b,a)) where
    Disabled b a = setDisabled b a