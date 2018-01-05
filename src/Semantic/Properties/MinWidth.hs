module Semantic.Properties.MinWidth where

import Semantic.Properties.Utils

class HasMinWidthProp a where
    getMinWidth :: a -> Int
    setMinWidth :: Int -> a -> a

pattern MinWidth :: HasMinWidthProp a => Int -> a -> a
pattern MinWidth mw a <- (getMinWidth &&& id -> (mw,a)) where
    MinWidth mw a = setMinWidth mw a