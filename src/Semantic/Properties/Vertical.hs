module Semantic.Properties.Vertical where

import Semantic.Properties.Utils

class HasVerticalProp a where
    getVertical :: a -> Bool
    setVertical :: Bool -> a -> a

pattern Vertical :: HasVerticalProp a => a -> a
pattern Vertical a <- (getVertical &&& id -> (True,a)) where
    Vertical a = setVertical True a