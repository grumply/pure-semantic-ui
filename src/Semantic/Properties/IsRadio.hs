module Semantic.Properties.IsRadio where

import Semantic.Properties.Utils

class HasIsRadioProp a where
    getIsRadio :: a -> Bool
    setIsRadio :: Bool -> a -> a

pattern IsRadio :: HasIsRadioProp a => a -> a
pattern IsRadio a <- (getIsRadio &&& id -> (True,a)) where
    IsRadio a = setIsRadio True a