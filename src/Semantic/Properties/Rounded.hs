module Semantic.Properties.Rounded where

import Semantic.Properties.Utils

class HasRoundedProp a where
    getRounded :: a -> Bool
    setRounded :: Bool -> a -> a

pattern Rounded :: HasRoundedProp a => a -> a
pattern Rounded a <- (getRounded &&& id -> (True,a)) where
    Rounded a = setRounded True a