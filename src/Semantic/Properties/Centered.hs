module Semantic.Properties.Centered where

import Semantic.Properties.Utils

class HasCenteredProp a where
    getCentered :: a -> Bool
    setCentered :: Bool -> a -> a

pattern Centered :: HasCenteredProp a => a -> a
pattern Centered a <- (getCentered &&& id -> (True,a)) where
    Centered a = setCentered True a