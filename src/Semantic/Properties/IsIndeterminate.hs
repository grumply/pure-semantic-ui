module Semantic.Properties.IsIndeterminate where

import Semantic.Properties.Utils

class HasIsIndeterminateProp a where
    getIsIndeterminate :: a -> Bool
    setIsIndeterminate :: Bool -> a -> a

pattern IsIndeterminate :: HasIsIndeterminateProp a => a -> a
pattern IsIndeterminate a <- (getIsIndeterminate &&& id -> (True,a)) where
    IsIndeterminate a = setIsIndeterminate True a