module Semantic.Properties.Floating where

import Semantic.Properties.Utils

class HasFloatingProp a where
    getFloating :: a -> Bool
    setFloating :: Bool -> a -> a

pattern Floating :: HasFloatingProp a => a -> a
pattern Floating a <- (getFloating &&& id -> (True,a)) where
    Floating a = setFloating True a