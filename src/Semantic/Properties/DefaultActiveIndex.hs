module Semantic.Properties.DefaultActiveIndex where

import Semantic.Properties.Utils

class HasDefaultActiveIndexProp a where
    getDefaultActiveIndex :: a -> Maybe Int
    setDefaultActiveIndex :: Maybe Int -> a -> a

pattern DefaultActiveIndex :: HasDefaultActiveIndexProp a => Maybe Int -> a -> a
pattern DefaultActiveIndex ai a <- (getDefaultActiveIndex &&& id -> (ai,a)) where
    DefaultActiveIndex ai a = setDefaultActiveIndex ai a