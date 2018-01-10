module Semantic.Properties.Upward where

import Semantic.Properties.Utils

class HasUpwardProp a where
    getUpward :: a -> Bool
    setUpward :: Bool -> a -> a

pattern Upward :: HasUpwardProp a => a -> a
pattern Upward a <- (getUpward &&& id -> (True,a)) where
    Upward a = setUpward True a