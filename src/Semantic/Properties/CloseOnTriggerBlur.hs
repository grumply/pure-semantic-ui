module Semantic.Properties.CloseOnTriggerBlur where

import Semantic.Properties.Utils

class HasCloseOnTriggerBlurProp a where
    getCloseOnTriggerBlur :: a -> Bool
    setCloseOnTriggerBlur :: Bool -> a -> a

pattern CloseOnTriggerBlur :: HasCloseOnTriggerBlurProp a => a -> a
pattern CloseOnTriggerBlur a <- (getCloseOnTriggerBlur &&& id -> (True,a)) where
    CloseOnTriggerBlur a = setCloseOnTriggerBlur True a