module Semantic.Properties.Piled where

import Semantic.Properties.Utils

class HasPiledProp a where
    getPiled :: a -> Bool
    setPiled :: Bool -> a -> a

pattern Piled :: HasPiledProp a => a -> a
pattern Piled a <- (getPiled &&& id -> (True,a)) where
    Piled a = setPiled True a