module Semantic.Properties.Avatar where

import Semantic.Properties.Utils

class HasAvatarProp a where
    getAvatar :: a -> Bool
    setAvatar :: Bool -> a -> a

pattern Avatar :: HasAvatarProp a => a -> a
pattern Avatar c <- (getAvatar &&& id -> (True,c)) where
    Avatar c = setAvatar True c