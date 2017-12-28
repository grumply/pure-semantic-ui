module Semantic.Properties.IsContainer where

import Semantic.Properties.Utils

class HasIsContainerProp a where
    getIsContainer :: a -> Bool
    setIsContainer :: Bool -> a -> a

pattern IsContainer :: HasIsContainerProp a => a -> a
pattern IsContainer a <- (getIsContainer &&& id -> (True,a)) where
    IsContainer a = setIsContainer True a