module Semantic.Properties.OnTablet where

import Semantic.Utils
import Semantic.Properties.Utils

class HasOnTabletProp a where
    getOnTablet :: a -> Width
    setOnTablet :: Width -> a -> a

pattern OnTablet :: HasOnTabletProp a => Width -> a -> a
pattern OnTablet w a <- (getOnTablet &&& id -> (w,a)) where
    OnTablet w a = setOnTablet w a