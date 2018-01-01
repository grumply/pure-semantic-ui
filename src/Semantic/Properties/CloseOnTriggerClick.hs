module Semantic.Properties.CloseOnTriggerClick where

import Semantic.Properties.Utils

class HasCloseOnTriggerClickProp a where
    getCloseOnTriggerClick :: a -> Bool
    setCloseOnTriggerClick :: Bool -> a -> a

pattern CloseOnTriggerClick :: HasCloseOnTriggerClickProp a => a -> a
pattern CloseOnTriggerClick a <- (getCloseOnTriggerClick &&& id -> (True,a)) where
    CloseOnTriggerClick a = setCloseOnTriggerClick True a