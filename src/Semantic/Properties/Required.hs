module Semantic.Properties.Required where

import Semantic.Properties.Utils

class HasRequiredProp a where
    getRequired :: a -> Bool
    setRequired :: Bool -> a -> a

pattern Required :: HasRequiredProp a => a -> a
pattern Required a <- (getRequired &&& id -> (True,a)) where
    Required a = setRequired True a