module Semantic.Properties.Celled where

import Semantic.Properties.Utils

class HasCelledProp a where
    type CelledProp a :: *
    type CelledProp a = Bool
    getCelled :: a -> CelledProp a
    setCelled :: CelledProp a -> a -> a

pattern Celled :: HasCelledProp a => CelledProp a -> a -> a
pattern Celled c a <- (getCelled &&& id -> (c,a)) where
    Celled c a = setCelled c a