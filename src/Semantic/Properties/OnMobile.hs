module Semantic.Properties.OnMobile where

import Semantic.Utils
import Semantic.Properties.Utils

class HasOnMobileProp a where
    getOnMobile :: a -> Width
    setOnMobile :: Width -> a -> a

pattern OnMobile :: HasOnMobileProp a => Width -> a -> a
pattern OnMobile w a <- (getOnMobile &&& id -> (w,a)) where
    OnMobile w a = setOnMobile w a