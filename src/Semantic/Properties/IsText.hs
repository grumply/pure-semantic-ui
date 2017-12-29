module Semantic.Properties.IsText where

import Semantic.Properties.Utils

class HasIsTextProp a where
    getIsText :: a -> Bool
    setIsText :: Bool -> a -> a

pattern IsText :: HasIsTextProp a => a -> a
pattern IsText a <- (getIsText &&& id -> (True,a)) where
    IsText a = setIsText True a