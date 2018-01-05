module Semantic.Properties.WithCheckbox where

import Semantic.Properties.Utils

class HasWithCheckboxProp a where
    type WithCheckboxProp a
    getWithCheckbox :: a -> WithCheckboxProp a
    setWithCheckbox :: WithCheckboxProp a -> a -> a

pattern WithCheckbox :: HasWithCheckboxProp a => WithCheckboxProp a -> a -> a
pattern WithCheckbox wm a <- (getWithCheckbox &&& id -> (wm,a)) where
    WithCheckbox wm a = setWithCheckbox wm a