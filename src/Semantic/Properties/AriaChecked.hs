module Semantic.Properties.AriaChecked where

import Semantic.Properties.Utils

class HasAriaCheckedProp a where
    getAriaChecked :: a -> Bool
    setAriaChecked :: Bool -> a -> a

pattern AriaChecked :: HasAriaCheckedProp a => Bool -> a -> a
pattern AriaChecked b a <- (getAriaChecked &&& id -> (b,a)) where
    AriaChecked b a = setAriaChecked b a