module Semantic.Properties.Active where

import Semantic.Properties.Utils

class HasActiveProp a where
    getActive :: a -> Bool
    setActive :: Bool -> a -> a

pattern Active :: HasActiveProp a => a -> a
pattern Active a <- (getActive &&& id -> (True,a)) where
    Active a = setActive True a
        
