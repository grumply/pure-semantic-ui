module Semantic.Properties.OnHide where

import Semantic.Properties.Utils

class HasOnHideProp a where
    type OnHideProp a
    getOnHide :: a -> OnHideProp a
    setOnHide :: OnHideProp a -> a -> a

pattern OnHide :: HasOnHideProp a => OnHideProp a -> a -> a
pattern OnHide oh a <- (getOnHide &&& id -> (oh,a)) where
    OnHide oh a = setOnHide oh a