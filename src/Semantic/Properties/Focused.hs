module Semantic.Properties.Focused where

import Semantic.Properties.Utils

class HasFocusedProp a where
    getFocused :: a -> Bool
    setFocused :: Bool -> a -> a

pattern Focused :: HasFocusedProp a => a -> a
pattern Focused a <- (getFocused &&& id -> (True,a)) where
    Focused a = setFocused True a