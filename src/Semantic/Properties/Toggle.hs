module Semantic.Properties.Toggle where

import Semantic.Properties.Utils

class HasToggleProp a where
    getToggle :: a -> Bool
    setToggle :: Bool -> a -> a

pattern Toggle :: HasToggleProp a => a -> a
pattern Toggle a <- (getToggle &&& id -> (True,a)) where
    Toggle a = setToggle True a