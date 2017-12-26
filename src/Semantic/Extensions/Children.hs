module Semantic.Extensions.Children where

import Semantic.Extensions.Utils

class HasChildren a where
    type Child a
    getChildren :: a -> [Child a]
    setChildren :: [Child a] -> a -> a

pattern Children :: HasChildren a => [Child a] -> a -> a
pattern Children cs a <- (getChildren &&& id -> (cs,a)) where
    Children cs a = setChildren cs a

infixl 1 !
(!) c cs = Children cs c

