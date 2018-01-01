module Semantic.Properties.Trigger where

import Semantic.Properties.Utils

class HasTriggerProp a where
    type TriggerProp a
    getTrigger :: a -> TriggerProp a
    setTrigger :: TriggerProp a -> a -> a

pattern Trigger :: HasTriggerProp a => TriggerProp a -> a -> a
pattern Trigger t a <- (getTrigger &&& id -> (t,a)) where
    Trigger t a = setTrigger t a