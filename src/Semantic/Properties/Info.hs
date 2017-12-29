module Semantic.Properties.Info where

import Semantic.Properties.Utils

class HasInfoProp a where
    getInfo :: a -> Bool
    setInfo :: Bool -> a -> a

pattern Info :: HasInfoProp a => a -> a
pattern Info a <- (getInfo &&& id -> (True,a)) where
    Info a = setInfo True a