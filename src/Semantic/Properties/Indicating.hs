module Semantic.Properties.Indicating where

import Semantic.Properties.Utils

class HasIndicatingProp a where
    getIndicating :: a -> Bool
    setIndicating :: Bool -> a -> a

pattern Indicating :: HasIndicatingProp a => a -> a
pattern Indicating a <- (getIndicating &&& id -> (True,a)) where
    Indicating a = setIndicating True a