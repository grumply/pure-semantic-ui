module Semantic.Properties.ActiveIndex where

import Semantic.Properties.Utils

class HasActiveIndexProp a where
    getActiveIndex :: a -> Maybe Int
    setActiveIndex :: Maybe Int -> a -> a

pattern ActiveIndex :: HasActiveIndexProp a => Maybe Int -> a -> a
pattern ActiveIndex ai a <- (getActiveIndex &&& id -> (ai,a)) where
    ActiveIndex ai a = setActiveIndex ai a