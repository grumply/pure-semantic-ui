module Semantic.Properties.WithTransition where

import Semantic.Properties.Utils

class HasWithTransitionProp a where
    type WithTransitionProp a
    getWithTransition :: a -> WithTransitionProp a
    setWithTransition :: WithTransitionProp a -> a -> a

pattern WithTransition :: HasWithTransitionProp a => WithTransitionProp a -> a -> a
pattern WithTransition wp a <- (getWithTransition &&& id -> (wp,a)) where
    WithTransition wp a = setWithTransition wp a