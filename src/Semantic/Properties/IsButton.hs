module Semantic.Properties.IsButton where

import Semantic.Properties.Utils

class HasIsButtonProp a where
    getIsButton :: a -> Bool
    setIsButton :: Bool -> a -> a

pattern IsButton :: HasIsButtonProp a => a -> a
pattern IsButton a <- (getIsButton &&& id -> (True,a)) where
    IsButton a = setIsButton True a