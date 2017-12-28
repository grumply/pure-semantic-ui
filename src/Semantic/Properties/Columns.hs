module Semantic.Properties.Columns where

import Semantic.Utils
import Semantic.Properties.Utils

class HasColumnsProp a where
    getColumns :: a -> Width
    setColumns :: Width -> a -> a

pattern Columns :: HasColumnsProp a => Width -> a -> a
pattern Columns w a <- (getColumns &&& id -> (w,a)) where
    Columns w a = setColumns w a