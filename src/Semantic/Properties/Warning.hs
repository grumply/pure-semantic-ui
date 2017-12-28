module Semantic.Properties.Warning where

import Semantic.Properties.Utils

class HasWarningProp a where
    getWarning :: a -> Bool
    setWarning :: Bool -> a -> a

pattern Warning :: HasWarningProp a => Bool -> a -> a
pattern Warning w a <- (getWarning &&& id -> (w,a)) where
    Warning w a = setWarning w a