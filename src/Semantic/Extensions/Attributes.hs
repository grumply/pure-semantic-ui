module Semantic.Extensions.Attributes where

import Semantic.Extensions.Utils

class HasAttributes a where
    type Attribute a
    getAttributes :: a -> [Attribute a]
    setAttributes :: [Attribute a] -> a -> a

pattern Attributes :: HasAttributes a => [Attribute a] -> a -> a
pattern Attributes cs a <- (getAttributes &&& id -> (cs,a)) where
    Attributes cs a = setAttributes cs a
       
infixl 2 %
(%) c as = Attributes as c

