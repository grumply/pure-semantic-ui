module Semantic.Properties.Multiple where

import Semantic.Properties.Utils

class HasMultipleProp a where
    getMultiple :: a -> Bool
    setMultiple :: Bool -> a -> a

pattern Multiple :: HasMultipleProp a => a -> a
pattern Multiple a <- (getMultiple &&& id -> (True,a)) where
    Multiple a = setMultiple True a