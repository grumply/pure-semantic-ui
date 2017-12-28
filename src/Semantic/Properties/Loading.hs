module Semantic.Properties.Loading where

import Semantic.Properties.Utils

class HasLoadingProp a where
    getLoading :: a -> Bool
    setLoading :: Bool -> a -> a

pattern Loading :: HasLoadingProp a => Bool -> a -> a
pattern Loading b a <- (getLoading &&& id -> (b,a)) where
    Loading b a = setLoading b a