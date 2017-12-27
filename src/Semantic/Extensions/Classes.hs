module Semantic.Extensions.Classes where

import Semantic.Extensions.Utils

class HasClasses a where
    getClasses :: a -> [Txt]
    setClasses :: [Txt] -> a -> a

pattern Classes :: HasClasses a => [Txt] -> a -> a
pattern Classes cs a <- (getClasses &&& id -> (cs,a)) where
    Classes cs a = setClasses cs a
