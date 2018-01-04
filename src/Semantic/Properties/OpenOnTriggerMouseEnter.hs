module Semantic.Properties.OpenOnTriggerMouseEnter where

import Semantic.Properties.Utils

class HasOpenOnTriggerMouseEnterProp a where
    getOpenOnTriggerMouseEnter :: a -> Bool
    setOpenOnTriggerMouseEnter :: Bool -> a -> a

pattern OpenOnTriggerMouseEnter :: HasOpenOnTriggerMouseEnterProp a => a -> a
pattern OpenOnTriggerMouseEnter a <- (getOpenOnTriggerMouseEnter &&& id -> (True,a)) where
    OpenOnTriggerMouseEnter a = setOpenOnTriggerMouseEnter True a

pattern NoOpenOnTriggerMouseEnter :: HasOpenOnTriggerMouseEnterProp a => a -> a
pattern NoOpenOnTriggerMouseEnter a <- (getOpenOnTriggerMouseEnter &&& id -> (False,a)) where
    NoOpenOnTriggerMouseEnter a = setOpenOnTriggerMouseEnter False a