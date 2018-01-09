module Semantic.Properties.Pushing where

import Semantic.Properties.Utils

class HasPushingProp a where
    getPushing :: a -> Bool
    setPushing :: Bool -> a -> a

pattern Pushing :: HasPushingProp a => a -> a
pattern Pushing a <- (getPushing &&& id -> (True,a)) where
    Pushing a = setPushing True a