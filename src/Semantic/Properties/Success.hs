module Semantic.Properties.Success where

import Semantic.Properties.Utils

class HasSuccessProp a where
    getSuccess :: a -> Bool
    setSuccess :: Bool -> a -> a

pattern Success :: HasSuccessProp a => Bool -> a -> a
pattern Success b a <- (getSuccess &&& id -> (b,a)) where
    Success b a = setSuccess b a