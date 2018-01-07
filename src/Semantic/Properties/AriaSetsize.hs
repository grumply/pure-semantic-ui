module Semantic.Properties.AriaSetsize where

import Semantic.Properties.Utils

class HasAriaSetsizeProp a where
    getAriaSetsize :: a -> Int
    setAriaSetsize :: Int -> a -> a

pattern AriaSetsize :: HasAriaSetsizeProp a => Int -> a -> a
pattern AriaSetsize i a <- (getAriaSetsize &&& id -> (i,a)) where
    AriaSetsize i a = setAriaSetsize i a