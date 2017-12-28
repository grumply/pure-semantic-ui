module Semantic.Properties.Grouped where

import Semantic.Properties.Utils

class HasGroupedProp a where
    getGrouped :: a -> Bool
    setGrouped :: Bool -> a -> a

pattern Grouped :: HasGroupedProp a => a -> a
pattern Grouped a <- (getGrouped &&& id -> (True,a)) where
    Grouped a = setGrouped True a