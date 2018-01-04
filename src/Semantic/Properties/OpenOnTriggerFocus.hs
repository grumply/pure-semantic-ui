module Semantic.Properties.OpenOnTriggerFocus where

import Semantic.Properties.Utils

class HasOpenOnTriggerFocusProp a where
    getOpenOnTriggerFocus :: a -> Bool
    setOpenOnTriggerFocus :: Bool -> a -> a

pattern OpenOnTriggerFocus :: HasOpenOnTriggerFocusProp a => a -> a
pattern OpenOnTriggerFocus a <- (getOpenOnTriggerFocus &&& id -> (True,a)) where
    OpenOnTriggerFocus a = setOpenOnTriggerFocus True a

pattern NoOpenOnTriggerFocus :: HasOpenOnTriggerFocusProp a => a -> a
pattern NoOpenOnTriggerFocus a <- (getOpenOnTriggerFocus &&& id -> (False,a)) where
    NoOpenOnTriggerFocus a = setOpenOnTriggerFocus False a