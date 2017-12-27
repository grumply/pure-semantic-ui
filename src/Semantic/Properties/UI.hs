module Semantic.Properties.UI where

import Semantic.Properties.Utils

class HasUIProp a where
    getUI :: a -> Bool
    setUI :: Bool -> a -> a

pattern UI :: HasUIProp a => a -> a
pattern UI a <- (getUI &&& id -> (True,a)) where
    UI a = setUI True a