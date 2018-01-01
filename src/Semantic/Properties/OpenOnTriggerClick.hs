module Semantic.Properties.OpenOnTriggerClick where

import Semantic.Properties.Utils

class HasOpenOnTriggerClickProp a where
    getOpenOnTriggerClick :: a -> Bool
    setOpenOnTriggerClick :: Bool -> a -> a

pattern OpenOnTriggerClick :: HasOpenOnTriggerClickProp a => Bool -> a -> a
pattern OpenOnTriggerClick o a <- (getOpenOnTriggerClick &&& id -> (o,a)) where
    OpenOnTriggerClick o a = setOpenOnTriggerClick o a