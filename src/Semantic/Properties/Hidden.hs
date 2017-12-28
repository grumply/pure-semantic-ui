module Semantic.Properties.Hidden where

import Semantic.Properties.Utils

class HasHiddenProp a where
    getHidden :: a -> Bool
    setHidden :: Bool -> a -> a

pattern Hidden :: HasHiddenProp a => Bool -> a -> a
pattern Hidden b a <- (getHidden &&& id -> (b,a)) where
    Hidden b a = setHidden b a