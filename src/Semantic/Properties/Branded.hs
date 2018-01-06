module Semantic.Properties.Branded where

import Semantic.Properties.Utils

class HasBrandedProp a where
    getBranded :: a -> Bool
    setBranded :: Bool -> a -> a

pattern Branded :: HasBrandedProp a => Bool -> a -> a
pattern Branded b a <- (getBranded &&& id -> (b,a)) where
    Branded b a = setBranded b a
        
