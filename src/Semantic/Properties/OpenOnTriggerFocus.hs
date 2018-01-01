module Semantic.Properties.OpenOnTriggerFocus where

import Semantic.Properties.Utils

class HasOpenOnTriggerFocusProp a where
    getOpenOnTriggerFocus :: a -> Bool
    setOpenOnTriggerFocus :: Bool -> a -> a

pattern OpenOnTriggerFocus :: HasOpenOnTriggerFocusProp a => Bool -> a -> a
pattern OpenOnTriggerFocus o a <- (getOpenOnTriggerFocus &&& id -> (o,a)) where
    OpenOnTriggerFocus o a = setOpenOnTriggerFocus o a