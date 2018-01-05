module Semantic.Properties.Width where

import Semantic.Utils
import Semantic.Properties.Utils

class HasWidthProp a where
    type WidthProp a :: *
    type WidthProp a = Width
    getWidth :: a -> WidthProp a
    setWidth :: WidthProp a -> a -> a

pattern Width :: HasWidthProp a => WidthProp a -> a -> a
pattern Width w a <- (getWidth &&& id -> (w,a)) where
    Width w a = setWidth w a