module Semantic.Properties.Only where

import Semantic.Properties.Utils

class HasOnlyProp a where
    getOnly :: a -> [Txt]
    setOnly :: [Txt] -> a -> a

pattern Only :: HasOnlyProp a => [Txt] -> a -> a
pattern Only w a <- (getOnly &&& id -> (w,a)) where
    Only w a = setOnly w a