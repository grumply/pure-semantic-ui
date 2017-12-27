module Semantic.Properties.Labeled where

import Semantic.Properties.Utils

class HasLabeledProp a where
    getLabeled :: a -> Bool
    setLabeled :: Bool -> a -> a

pattern Labeled :: HasLabeledProp a => a -> a
pattern Labeled a <- (getLabeled &&& id -> (True,a)) where
    Labeled a = setLabeled True a