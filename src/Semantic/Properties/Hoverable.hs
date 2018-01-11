module Semantic.Properties.Hoverable where

import Semantic.Properties.Utils

class HasHoverableProp a where
    getHoverable :: a -> Bool
    setHoverable :: Bool -> a -> a

pattern Hoverable :: HasHoverableProp a => a -> a
pattern Hoverable a <- (getHoverable &&& id -> (True,a)) where
    Hoverable a = setHoverable True a