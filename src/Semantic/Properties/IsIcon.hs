module Semantic.Properties.IsIcon where

import Semantic.Properties.Utils

class HasIsIconProp a where
    getIsIcon :: a -> Maybe Txt
    setIsIcon :: Maybe Txt -> a -> a

pattern IsIcon :: HasIsIconProp a => Maybe Txt -> a -> a
pattern IsIcon i a <- (getIsIcon &&& id -> (i,a)) where
    IsIcon i a = setIsIcon i a