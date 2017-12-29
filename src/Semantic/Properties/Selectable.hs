module Semantic.Properties.Selectable where

import Semantic.Properties.Utils

class HasSelectableProp a where
    getSelectable :: a -> Bool
    setSelectable :: Bool -> a -> a

pattern Selectable :: HasSelectableProp a => a -> a
pattern Selectable a <- (getSelectable &&& id -> (True,a)) where
    Selectable a = setSelectable True a
