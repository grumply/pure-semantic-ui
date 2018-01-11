module Semantic.Properties.Wide where

import Semantic.Properties.Utils

class HasWideProp a where
    getWide :: a -> Maybe Txt
    setWide :: Maybe Txt -> a -> a

pattern Wide :: HasWideProp a => Maybe Txt -> a -> a
pattern Wide w a <- (getWide &&& id -> (w,a)) where
    Wide w a = setWide w a