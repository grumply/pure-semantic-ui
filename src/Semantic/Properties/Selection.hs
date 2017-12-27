module Semantic.Properties.Selection where

import Semantic.Properties.Utils

class HasSelectionProp a where
    getSelection :: a -> Bool
    setSelection :: Bool -> a -> a

pattern Selection :: HasSelectionProp a => a -> a
pattern Selection a <- (getSelection &&& id -> (True,a)) where
    Selection a = setSelection True a