module Semantic.Properties.Percent where

import Semantic.Properties.Utils

class HasPercentProp a where
    getPercent :: a -> Maybe Double
    setPercent :: Maybe Double -> a -> a

pattern Percent :: HasPercentProp a => Double -> a -> a
pattern Percent p a <- (getPercent &&& id -> (Just p,a)) where
    Percent p a = setPercent (Just p) a