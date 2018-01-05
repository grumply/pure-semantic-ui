module Semantic.Properties.AutoHeight where

import Semantic.Properties.Utils

class HasAutoHeightProp a where
    getAutoHeight :: a -> Bool
    setAutoHeight :: Bool -> a -> a

pattern AutoHeight :: HasAutoHeightProp a => a -> a
pattern AutoHeight a <- (getAutoHeight &&& id -> (True,a)) where
    AutoHeight a = setAutoHeight True a
        
