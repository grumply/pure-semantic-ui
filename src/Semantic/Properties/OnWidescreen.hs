module Semantic.Properties.OnWidescreen where

import Semantic.Utils
import Semantic.Properties.Utils

class HasOnWidescreenProp a where
    getOnWidescreen :: a -> Width
    setOnWidescreen :: Width -> a -> a

pattern OnWidescreen :: HasOnWidescreenProp a => Width -> a -> a
pattern OnWidescreen w a <- (getOnWidescreen &&& id -> (w,a)) where
    OnWidescreen w a = setOnWidescreen w a