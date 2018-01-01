module Semantic.Properties.MouseLeaveDelay where

import Semantic.Properties.Utils

class HasMouseLeaveDelayProp a where
    getMouseLeaveDelay :: a -> Int
    setMouseLeaveDelay :: Int -> a -> a

pattern MouseLeaveDelay :: HasMouseLeaveDelayProp a => Int -> a -> a
pattern MouseLeaveDelay d a <- (getMouseLeaveDelay &&& id -> (d,a)) where
    MouseLeaveDelay d a = setMouseLeaveDelay d a