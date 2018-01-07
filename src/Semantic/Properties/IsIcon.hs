module Semantic.Properties.IsIcon where

import Semantic.Properties.Utils

class HasIsIconProp a where
    type IsIconProp a :: *
    type IsIconProp a = Maybe Txt
    getIsIcon :: a -> IsIconProp a
    setIsIcon :: IsIconProp a -> a -> a

pattern IsIcon :: HasIsIconProp a => IsIconProp a -> a -> a
pattern IsIcon i a <- (getIsIcon &&& id -> (i,a)) where
    IsIcon i a = setIsIcon i a