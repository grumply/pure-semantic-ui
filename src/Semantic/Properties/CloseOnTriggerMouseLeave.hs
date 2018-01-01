module Semantic.Properties.CloseOnTriggerMouseLeave where

import Semantic.Properties.Utils

class HasCloseOnTriggerMouseLeaveProp a where
    getCloseOnTriggerMouseLeave :: a -> Bool
    setCloseOnTriggerMouseLeave :: Bool -> a -> a

pattern CloseOnTriggerMouseLeave :: HasCloseOnTriggerMouseLeaveProp a => a -> a
pattern CloseOnTriggerMouseLeave a <- (getCloseOnTriggerMouseLeave &&& id -> (True,a)) where
    CloseOnTriggerMouseLeave a = setCloseOnTriggerMouseLeave True a