module Semantic.Properties.TransitionOnMount where

import Semantic.Properties.Utils

class HasTransitionOnMountProp a where
    getTransitionOnMount :: a -> Bool
    setTransitionOnMount :: Bool -> a -> a

pattern TransitionOnMount :: HasTransitionOnMountProp a => a -> a
pattern TransitionOnMount a <- (getTransitionOnMount &&& id -> (True,a)) where
    TransitionOnMount a = setTransitionOnMount True a