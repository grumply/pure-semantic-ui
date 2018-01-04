module Semantic.Properties.Dimmed where

import Semantic.Properties.Utils

class HasDimmedProp a where
    getDimmed :: a -> Bool
    setDimmed :: Bool -> a -> a

pattern Dimmed :: HasDimmedProp a => Bool -> a -> a
pattern Dimmed b a <- (getDimmed &&& id -> (b,a)) where
    Dimmed b a = setDimmed b a