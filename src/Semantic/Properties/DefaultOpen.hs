module Semantic.Properties.DefaultOpen where

import Semantic.Properties.Utils

class HasDefaultOpenProp a where
    getDefaultOpen :: a -> Bool
    setDefaultOpen :: Bool -> a -> a

pattern DefaultOpen :: HasDefaultOpenProp a => a -> a
pattern DefaultOpen a <- (getDefaultOpen &&& id -> (True,a)) where
    DefaultOpen a = setDefaultOpen True a