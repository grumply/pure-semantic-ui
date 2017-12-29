module Semantic.Properties.Definition where

import Semantic.Properties.Utils

class HasDefinitionProp a where
    getDefinition :: a -> Bool
    setDefinition :: Bool -> a -> a

pattern Definition :: HasDefinitionProp a => a -> a
pattern Definition a <- (getDefinition &&& id -> (True,a)) where
    Definition a = setDefinition True a