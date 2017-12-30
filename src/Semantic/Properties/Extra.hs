module Semantic.Properties.Extra where

import Semantic.Properties.Utils

class HasExtraProp a where
    getExtra :: a -> Bool
    setExtra :: Bool -> a -> a

pattern Extra :: HasExtraProp a => a -> a
pattern Extra a <- (getExtra &&& id -> (True,a)) where
    Extra a = setExtra True a