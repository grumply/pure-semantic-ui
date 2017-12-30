module Semantic.Properties.Collapsed where

import Semantic.Properties.Utils

class HasCollapsedProp a where
    getCollapsed :: a -> Bool
    setCollapsed :: Bool -> a -> a

pattern Collapsed :: HasCollapsedProp a => Bool -> a -> a
pattern Collapsed c a <- (getCollapsed &&& id -> (c,a)) where
    Collapsed c a = setCollapsed c a