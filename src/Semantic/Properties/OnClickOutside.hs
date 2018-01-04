module Semantic.Properties.OnClickOutside where

import Semantic.Properties.Utils

class HasOnClickOutsideProp a where
    type OnClickOutsideProp a
    getOnClickOutside :: a -> OnClickOutsideProp a
    setOnClickOutside :: OnClickOutsideProp a -> a -> a

pattern OnClickOutside :: HasOnClickOutsideProp a => OnClickOutsideProp a -> a -> a
pattern OnClickOutside occ a <- (getOnClickOutside &&& id -> (occ,a)) where
    OnClickOutside occ a = setOnClickOutside occ a