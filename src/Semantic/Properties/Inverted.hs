module Semantic.Properties.Inverted where

import Semantic.Properties.Utils

class HasInvertedProp a where
    getInverted :: a -> Bool
    setInverted :: Bool -> a -> a

pattern Inverted :: HasInvertedProp a => a -> a
pattern Inverted a <- (getInverted &&& id -> (True,a)) where
    Inverted a = setInverted True a