module Semantic.Properties.Width where

import Semantic.Utils
import Semantic.Properties.Utils

class HasWidthProp a where
    getWidth :: a -> Width
    setWidth :: Width -> a -> a

pattern Width :: HasWidthProp a => Width -> a -> a
pattern Width w a <- (getWidth &&& id -> (w,a)) where
    Width w a = setWidth w a