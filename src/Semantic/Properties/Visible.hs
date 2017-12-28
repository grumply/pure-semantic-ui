module Semantic.Properties.Visible where

import Semantic.Properties.Utils

class HasVisibleProp a where
    getVisible :: a -> Bool
    setVisible :: Bool -> a -> a

pattern Visible :: HasVisibleProp a => Bool -> a -> a
pattern Visible b a <- (getVisible &&& id -> (b,a)) where
    Visible b a = setVisible b a

