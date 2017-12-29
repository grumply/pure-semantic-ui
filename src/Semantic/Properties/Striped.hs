module Semantic.Properties.Striped where

import Semantic.Properties.Utils

class HasStripedProp a where
    getStriped :: a -> Bool
    setStriped :: Bool -> a -> a

pattern Striped :: HasStripedProp a => a -> a
pattern Striped a <- (getStriped &&& id -> (True,a)) where
    Striped a = setStriped True a