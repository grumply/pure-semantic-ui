module Semantic.Properties.Collapsing where

import Semantic.Properties.Utils

class HasCollapsingProp a where
    getCollapsing :: a -> Bool
    setCollapsing :: Bool -> a -> a

pattern Collapsing :: HasCollapsingProp a => a -> a
pattern Collapsing a <- (getCollapsing &&& id -> (True,a)) where
    Collapsing a = setCollapsing True a