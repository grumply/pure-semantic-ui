module Semantic.Properties.Error where

import Semantic.Properties.Utils

class HasErrorProp a where
    getError :: a -> Bool
    setError :: Bool -> a -> a

pattern Error :: HasErrorProp a => a -> a
pattern Error a <- (getError &&& id -> (True,a)) where
    Error a = setError True a