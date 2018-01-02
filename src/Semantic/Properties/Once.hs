module Semantic.Properties.Once where

import Semantic.Properties.Utils

class HasOnceProp a where
    getOnce :: a -> Bool
    setOnce :: Bool -> a -> a

pattern Once :: HasOnceProp a => a -> a
pattern Once a <- (getOnce &&& id -> (True,a)) where
    Once a = setOnce True a