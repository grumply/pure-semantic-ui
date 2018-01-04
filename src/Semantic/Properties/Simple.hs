module Semantic.Properties.Simple where

import Semantic.Properties.Utils

class HasSimpleProp a where
    getSimple :: a -> Bool
    setSimple :: Bool -> a -> a

pattern Simple :: HasSimpleProp a => a -> a
pattern Simple a <- (getSimple &&& id -> (True,a)) where
    Simple a = setSimple True a 