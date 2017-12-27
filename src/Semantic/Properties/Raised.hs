module Semantic.Properties.Raised where

import Semantic.Properties.Utils

class HasRaisedProp a where
    getRaised :: a -> Bool
    setRaised :: Bool -> a -> a

pattern Raised :: HasRaisedProp a => a -> a
pattern Raised a <- (getRaised &&& id -> (True,a)) where
    Raised a = setRaised True a