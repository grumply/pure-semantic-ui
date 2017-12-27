module Semantic.Properties.Transition where

import Semantic.Properties.Utils

class HasTransitionProp a where
    getTransition :: a -> Txt
    setTransition :: Txt -> a -> a

pattern Transition :: HasTransitionProp a => Txt -> a -> a
pattern Transition t a <- (getTransition &&& id -> (t,a)) where
    Transition t a = setTransition t a
