module Semantic.Properties.MaxWidth where

import Semantic.Properties.Utils

class HasMaxWidthProp a where
    getMaxWidth :: a -> Int
    setMaxWidth :: Int -> a -> a

pattern MaxWidth :: HasMaxWidthProp a => Int -> a -> a
pattern MaxWidth mw a <- (getMaxWidth &&& id -> (mw,a)) where
    MaxWidth mw a = setMaxWidth mw a