module Semantic.Properties.Structured where

import Semantic.Properties.Utils

class HasStructuredProp a where
    getStructured :: a -> Bool
    setStructured :: Bool -> a -> a

pattern Structured :: HasStructuredProp a => a -> a
pattern Structured a <- (getStructured &&& id -> (True,a)) where
    Structured a = setStructured True a