module Semantic.Properties.Error where

import Semantic.Properties.Utils

class HasErrorProp a where
    getError :: a -> Bool
    setError :: Bool -> a -> a

pattern Error :: HasErrorProp a => Bool -> a -> a
pattern Error b a <- (getError &&& id -> (b,a)) where
    Error b a = setError b a