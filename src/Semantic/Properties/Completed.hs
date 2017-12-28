module Semantic.Properties.Completed where

import Semantic.Properties.Utils

class HasCompletedProp a where
    getCompleted :: a -> Bool
    setCompleted :: Bool -> a -> a

pattern Completed :: HasCompletedProp a => Bool -> a -> a
pattern Completed b a <- (getCompleted &&& id -> (b,a)) where
    Completed b a = setCompleted b a