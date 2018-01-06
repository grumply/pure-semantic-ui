module Semantic.Properties.DefaultActive where

import Semantic.Properties.Utils

class HasDefaultActiveProp a where
    getDefaultActive :: a -> Bool
    setDefaultActive :: Bool -> a -> a

pattern DefaultActive :: HasDefaultActiveProp a => a -> a
pattern DefaultActive a <- (getDefaultActive &&& id -> (True,a)) where
    DefaultActive a = setDefaultActive True a