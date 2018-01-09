module Semantic.Properties.OnUnstick where

import Semantic.Properties.Utils

class HasOnUnstickProp a where
    type OnUnstickProp a
    getOnUnstick :: a -> OnUnstickProp a
    setOnUnstick :: OnUnstickProp a -> a -> a

pattern OnUnstick :: HasOnUnstickProp a => OnUnstickProp a -> a -> a
pattern OnUnstick ou a <- (getOnUnstick &&& id -> (ou,a)) where
    OnUnstick ou a = setOnUnstick ou a