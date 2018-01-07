module Semantic.Properties.Selected where

import Semantic.Properties.Utils

class HasSelectedProp a where
    getSelected :: a -> Bool
    setSelected :: Bool -> a -> a

pattern Selected :: HasSelectedProp a => Bool -> a -> a
pattern Selected b a <- (getSelected &&& id -> (b,a)) where
    Selected b a = setSelected b a

