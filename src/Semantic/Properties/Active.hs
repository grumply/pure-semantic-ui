module Semantic.Properties.Active where

import Semantic.Properties.Utils

class HasActiveProp a where
    getActive :: a -> Bool
    setActive :: Bool -> a -> a

pattern Active :: HasActiveProp a => Bool -> a -> a
pattern Active b a <- (getActive &&& id -> (b,a)) where
    Active b a = setActive b a
        
