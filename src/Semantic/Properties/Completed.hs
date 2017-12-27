module Semantic.Properties.Completed where

import Semantic.Properties.Utils

class HasCompletedProp a where
    getCompleted :: a -> Bool
    setCompleted :: Bool -> a -> a

pattern Completed :: HasCompletedProp a => a -> a
pattern Completed a <- (getCompleted &&& id -> (True,a)) where
    Completed a = setCompleted True a