module Semantic.Properties.Wrapped where

import Semantic.Properties.Utils

class HasWrappedProp a where
    getWrapped :: a -> Bool
    setWrapped :: Bool -> a -> a

pattern Wrapped :: HasWrappedProp a => a -> a
pattern Wrapped a <- (getWrapped &&& id -> (True,a)) where
    Wrapped a = setWrapped True a