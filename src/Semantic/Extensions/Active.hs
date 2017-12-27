module Semantic.Extensions.Active where

import Semantic.Extensions.Utils

class HasActive a where
    getActive :: a -> Bool
    setActive :: Bool -> a -> a

pattern Active :: HasActive a => a -> a
pattern Active a <- (getActive &&& id -> (True,a)) where
    Active a = setActive True a
        
