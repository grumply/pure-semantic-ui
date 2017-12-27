module Semantic.Properties.Visible where

import Semantic.Properties.Utils

class HasVisibleProp a where
    getVisible :: a -> Bool
    setVisible :: Bool -> a -> a

pattern Visible :: HasVisibleProp a => a -> a
pattern Visible a <- (getVisible &&& id -> (True,a)) where
    Visible a = setVisible True a

