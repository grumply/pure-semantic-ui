module Semantic.Properties.Disabled where

import Semantic.Properties.Utils

class HasDisabledProp a where
    getDisabled :: a -> Bool
    setDisabled :: Bool -> a -> a

pattern Disabled :: HasDisabledProp a => a -> a
pattern Disabled a <- (getDisabled &&& id -> (True,a)) where
    Disabled a = setDisabled True a