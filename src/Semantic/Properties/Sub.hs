module Semantic.Properties.Sub where

import Semantic.Properties.Utils

class HasSubProp a where
    getSub :: a -> Bool
    setSub :: Bool -> a -> a

pattern Sub :: HasSubProp a => a -> a
pattern Sub a <- (getSub &&& id -> (True,a)) where
    Sub a = setSub True a
