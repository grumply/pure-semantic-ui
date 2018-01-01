module Semantic.Properties.MouseEnterDelay where

import Semantic.Properties.Utils

class HasMouseEnterDelayProp a where
    getMouseEnterDelay :: a -> Int
    setMouseEnterDelay :: Int -> a -> a

pattern MouseEnterDelay :: HasMouseEnterDelayProp a => Int -> a -> a
pattern MouseEnterDelay d a <- (getMouseEnterDelay &&& id -> (d,a)) where
    MouseEnterDelay d a = setMouseEnterDelay d a