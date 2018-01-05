module Semantic.Properties.ConfirmButton where

import Semantic.Properties.Utils

class HasConfirmButtonProp a where
    type ConfirmButtonProp a 
    getConfirmButton :: a -> ConfirmButtonProp a
    setConfirmButton :: ConfirmButtonProp a -> a -> a

pattern ConfirmButton :: HasConfirmButtonProp a => ConfirmButtonProp a -> a -> a
pattern ConfirmButton cb a <- (getConfirmButton &&& id -> (cb,a)) where
    ConfirmButton cb a = setConfirmButton cb a