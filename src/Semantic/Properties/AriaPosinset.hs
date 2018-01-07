module Semantic.Properties.AriaPosinset where

import Semantic.Properties.Utils

class HasAriaPosinsetProp a where
    getAriaPosinset :: a -> Int
    setAriaPosinset :: Int -> a -> a

pattern AriaPosinset :: HasAriaPosinsetProp a => Int -> a -> a
pattern AriaPosinset i a <- (getAriaPosinset &&& id -> (i,a)) where
    AriaPosinset i a = setAriaPosinset i a