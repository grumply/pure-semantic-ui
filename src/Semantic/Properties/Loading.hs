module Semantic.Properties.Loading where

import Semantic.Properties.Utils

class HasLoadingProp a where
    getLoading :: a -> Bool
    setLoading :: Bool -> a -> a

pattern Loading :: HasLoadingProp a => a -> a
pattern Loading a <- (getLoading &&& id -> (True,a)) where
    Loading a = setLoading True a