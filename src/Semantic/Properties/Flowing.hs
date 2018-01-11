module Semantic.Properties.Flowing where

import Semantic.Properties.Utils

class HasFlowingProp a where
    getFlowing :: a -> Bool
    setFlowing :: Bool -> a -> a

pattern Flowing :: HasFlowingProp a => a -> a
pattern Flowing a <- (getFlowing &&& id -> (True,a)) where
    Flowing a = setFlowing True a