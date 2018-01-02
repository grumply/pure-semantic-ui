module Semantic.Properties.Continuous where

import Semantic.Properties.Utils

class HasContinuousProp a where
    getContinuous :: a -> Bool
    setContinuous :: Bool -> a -> a

pattern Continuous :: HasContinuousProp a => a -> a
pattern Continuous a <- (getContinuous &&& id -> (True,a)) where
    Continuous a = setContinuous True a