module Semantic.Properties.BottomOffset where

import Semantic.Properties.Utils

class HasBottomOffsetProp a where
    getBottomOffset :: a -> Double
    setBottomOffset :: Double -> a -> a

pattern BottomOffset :: HasBottomOffsetProp a => Double -> a -> a
pattern BottomOffset mw a <- (getBottomOffset &&& id -> (mw,a)) where
    BottomOffset mw a = setBottomOffset mw a