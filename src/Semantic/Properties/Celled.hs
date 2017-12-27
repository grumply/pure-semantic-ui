module Semantic.Properties.Celled where

import Semantic.Properties.Utils

class HasCelledProp a where
    getCelled :: a -> Bool
    setCelled :: Bool -> a -> a

pattern Celled :: HasCelledProp a => a -> a
pattern Celled a <- (getCelled &&& id -> (True,a)) where
    Celled a = setCelled True a