module Semantic.Properties.Children where

import Pure.View

import Semantic.Properties.Utils

import Data.Function ((&))

class HasChildrenProp a where
    type Child a
    getChildren :: a -> [Child a]
    setChildren :: [Child a] -> a -> a

pattern Children :: HasChildrenProp a => [Child a] -> a -> a
pattern Children cs a <- (getChildren &&& id -> (cs,a)) where
    Children cs a = setChildren (getChildren a ++ cs) a

infixl 1 !
(!) c cs = Children cs c

infixl 1 |>
(|>) c cs = Children cs c 