module Semantic.Properties.OnFocus where

import Semantic.Properties.Utils

class HasOnFocusProp a where
    type OnFocusProp a
    getOnFocus :: a -> OnFocusProp a
    setOnFocus :: OnFocusProp a -> a -> a

pattern OnFocus :: HasOnFocusProp a => OnFocusProp a -> a -> a
pattern OnFocus onf a <- (getOnFocus &&& id -> (onf,a)) where
    OnFocus onf a = setOnFocus onf a