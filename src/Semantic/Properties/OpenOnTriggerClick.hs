module Semantic.Properties.OpenOnTriggerClick where

import Semantic.Properties.Utils

class HasOpenOnTriggerClickProp a where
    getOpenOnTriggerClick :: a -> Bool
    setOpenOnTriggerClick :: Bool -> a -> a

pattern OpenOnTriggerClick :: HasOpenOnTriggerClickProp a => a -> a
pattern OpenOnTriggerClick a <- (getOpenOnTriggerClick &&& id -> (True,a)) where
    OpenOnTriggerClick a = setOpenOnTriggerClick True a

pattern NoOpenOnTriggerClick :: HasOpenOnTriggerClickProp a => a -> a
pattern NoOpenOnTriggerClick a <- (getOpenOnTriggerClick &&& id -> (False,a)) where
    NoOpenOnTriggerClick a = setOpenOnTriggerClick False a