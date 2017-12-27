module Semantic.Properties.Tag where

import Semantic.Properties.Utils

class HasTagProp a where
    getTag :: a -> Bool
    setTag :: Bool -> a -> a

pattern Tag :: HasTagProp a => a -> a
pattern Tag a <- (getTag &&& id -> (True,a)) where
    Tag a = setTag True a