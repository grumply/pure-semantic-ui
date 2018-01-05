module Semantic.Properties.CancelButton where

import Semantic.Properties.Utils

class HasCancelButtonProp a where
    type CancelButtonProp a 
    getCancelButton :: a -> CancelButtonProp a
    setCancelButton :: CancelButtonProp a -> a -> a

pattern CancelButton :: HasCancelButtonProp a => CancelButtonProp a -> a -> a
pattern CancelButton cb a <- (getCancelButton &&& id -> (cb,a)) where
    CancelButton cb a = setCancelButton cb a