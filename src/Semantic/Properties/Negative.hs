module Semantic.Properties.Negative where

import Semantic.Properties.Utils

class HasNegativeProp a where
    getNegative :: a -> Bool
    setNegative :: Bool -> a -> a

pattern Negative :: HasNegativeProp a => a -> a
pattern Negative a <- (getNegative &&& id -> (True,a)) where
    Negative a = setNegative True a