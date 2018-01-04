module Semantic.Properties.CloseOnEscape where

import Semantic.Properties.Utils

class HasCloseOnEscapeProp a where
    getCloseOnEscape :: a -> Bool
    setCloseOnEscape :: Bool -> a -> a

pattern CloseOnEscape :: HasCloseOnEscapeProp a => a -> a
pattern CloseOnEscape a <- (getCloseOnEscape &&& id -> (True,a)) where
    CloseOnEscape a = setCloseOnEscape True a

pattern NoCloseOnEscape :: HasCloseOnEscapeProp a => a -> a
pattern NoCloseOnEscape a <- (getCloseOnEscape &&& id -> (False,a)) where
    NoCloseOnEscape a = setCloseOnEscape False a