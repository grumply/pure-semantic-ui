module Semantic.Properties.Clearing where

import Semantic.Properties.Utils

class HasClearingProp a where
    getClearing :: a -> Bool
    setClearing :: Bool -> a -> a

pattern Clearing :: HasClearingProp a => a -> a
pattern Clearing a <- (getClearing &&& id -> (True,a)) where
    Clearing a = setClearing True a