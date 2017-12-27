module Semantic.Properties.Color where

import Semantic.Properties.Utils

class HasColorProp a where
    getColor :: a -> Txt
    setColor :: Txt -> a -> a

pattern Color :: HasColorProp a => Txt -> a -> a
pattern Color c a <- (getColor &&& id -> (c,a)) where
    Color c a = setColor c a

pattern Red = "red"
pattern Orange = "orange"
pattern Yellow = "yellow"
pattern Olive = "olive"
pattern Green = "green"
pattern Teal = "teal"
pattern Blue = "blue"
pattern Violet = "violet"
pattern Purple = "purple"
pattern Pink = "pink"
pattern Brown = "brown"
pattern Grey = "grey"
pattern Gray = "grey"
pattern Black = "black"

