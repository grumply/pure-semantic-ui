module Semantic.Properties.Focus where

import Semantic.Properties.Utils

class HasFocusProp a where
    getFocus :: a -> Bool
    setFocus :: Bool -> a -> a

pattern Focus :: HasFocusProp a => a -> a
pattern Focus a <- (getFocus &&& id -> (True,a)) where
    Focus a = setFocus True a