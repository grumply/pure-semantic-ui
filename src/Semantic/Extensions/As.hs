module Semantic.Extensions.As where

import Semantic.Extensions.Utils
    
class HasAs a where
    type Constructor a
    getAs :: a -> Constructor a
    setAs :: Constructor a -> a -> a

pattern As :: HasAs a => Constructor a -> a -> a
pattern As as a <- (getAs &&& id -> (as,a)) where
    As as a = setAs as a
