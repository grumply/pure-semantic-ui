module Semantic.Extensions.Avatar where

import Semantic.Extensions.Utils

class HasAvatar a where
    getAvatar :: a -> Bool
    setAvatar :: Bool -> a -> a

pattern Avatar :: HasAvatar a => a -> a
pattern Avatar c <- (getAvatar &&& id -> (True,c)) where
    Avatar c = setAvatar True c