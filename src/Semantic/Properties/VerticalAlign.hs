module Semantic.Properties.VerticalAlign where

import Semantic.Properties.Utils

class HasVerticalAlignProp a where
    getVerticalAlign :: a -> Txt
    setVerticalAlign :: Txt -> a -> a

pattern VerticalAlign :: HasVerticalAlignProp a => Txt -> a -> a
pattern VerticalAlign v a <- (getVerticalAlign &&& id -> (v,a)) where
    VerticalAlign v a = setVerticalAlign v a