module Semantic.Properties.OpenOnTriggerMouseEnter where

import Semantic.Properties.Utils

class HasOpenOnTriggerMouseEnterProp a where
    getOpenOnTriggerMouseEnter :: a -> Bool
    setOpenOnTriggerMouseEnter :: Bool -> a -> a

pattern OpenOnTriggerMouseEnter :: HasOpenOnTriggerMouseEnterProp a => Bool -> a -> a
pattern OpenOnTriggerMouseEnter o a <- (getOpenOnTriggerMouseEnter &&& id -> (o,a)) where
    OpenOnTriggerMouseEnter o a = setOpenOnTriggerMouseEnter o a