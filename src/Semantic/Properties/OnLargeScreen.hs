module Semantic.Properties.OnLargeScreen where

import Semantic.Utils
import Semantic.Properties.Utils

class HasOnLargeScreenProp a where
    getOnLargeScreen :: a -> Width
    setOnLargeScreen :: Width -> a -> a

pattern OnLargeScreen :: HasOnLargeScreenProp a => Width -> a -> a
pattern OnLargeScreen w a <- (getOnLargeScreen &&& id -> (w,a)) where
    OnLargeScreen w a = setOnLargeScreen w a