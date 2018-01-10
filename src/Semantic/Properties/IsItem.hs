module Semantic.Properties.IsItem where

import Semantic.Properties.Utils

class HasIsItemProp a where
    getIsItem :: a -> Bool
    setIsItem :: Bool -> a -> a

pattern IsItem :: HasIsItemProp a => a -> a
pattern IsItem a <- (getIsItem &&& id -> (True,a)) where
    IsItem a = setIsItem True a