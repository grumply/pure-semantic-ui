module Semantic.Properties.Radio where

import Semantic.Properties.Utils

class HasRadioProp a where
    getRadio :: a -> Bool
    setRadio :: Bool -> a -> a

pattern Radio :: HasRadioProp a => a -> a
pattern Radio a <- (getRadio &&& id -> (True,a)) where
    Radio a = setRadio True a