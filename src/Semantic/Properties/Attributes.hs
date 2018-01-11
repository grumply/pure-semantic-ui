module Semantic.Properties.Attributes where

import Pure.View

import Semantic.Properties.Utils

import Data.Function ((&))

class HasAttributesProp a where
    type Attribute a
    getAttributes :: a -> [Attribute a]
    setAttributes :: [Attribute a] -> a -> a

pattern Attributes :: HasAttributesProp a => [Attribute a] -> a -> a
pattern Attributes cs a <- (getAttributes &&& id -> (cs,a)) where
    Attributes cs a = setAttributes (getAttributes a ++ cs) a

infixl 1 %
(%) c as = c & Attributes as