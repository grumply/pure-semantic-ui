module Semantic.Properties.Category where

import Semantic.Properties.Utils

class HasCategoryProp a where
    getCategory :: a -> Bool
    setCategory :: Bool -> a -> a

pattern Category :: HasCategoryProp a => a -> a
pattern Category a <- (getCategory &&& id -> (True,a)) where
    Category a = setCategory True a