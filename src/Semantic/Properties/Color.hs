module Semantic.Properties.Color where

import Semantic.Properties.Utils

class HasColorProp a where
    getColor :: a -> Txt
    setColor :: Txt -> a -> a

pattern Color :: HasColorProp a => Txt -> a -> a
pattern Color c a <- (getColor &&& id -> (c,a)) where
    Color c a = setColor c a
