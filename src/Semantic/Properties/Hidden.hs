module Semantic.Properties.Hidden where

import Semantic.Properties.Utils

class HasHiddenProp a where
    getHidden :: a -> Bool
    setHidden :: Bool -> a -> a

pattern Hidden :: HasHiddenProp a => a -> a
pattern Hidden a <- (getHidden &&& id -> (True,a)) where
    Hidden a = setHidden True a