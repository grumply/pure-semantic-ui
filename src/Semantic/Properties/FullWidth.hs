module Semantic.Properties.FullWidth where

import Semantic.Properties.Utils

class HasFullWidthProp a where
    getFullWidth :: a -> Bool
    setFullWidth :: Bool -> a -> a

pattern FullWidth :: HasFullWidthProp a => a -> a
pattern FullWidth a <- (getFullWidth &&& id -> (True,a)) where
    FullWidth a = setFullWidth True a