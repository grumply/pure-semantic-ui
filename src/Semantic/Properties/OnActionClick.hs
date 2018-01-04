module Semantic.Properties.OnActionClick where

import Semantic.Properties.Utils

class HasOnActionClickProp a where
    type OnActionClickProp a
    getOnActionClick :: a -> OnActionClickProp a
    setOnActionClick :: OnActionClickProp a -> a -> a

pattern OnActionClick :: HasOnActionClickProp a => OnActionClickProp a -> a -> a
pattern OnActionClick oac a <- (getOnActionClick &&& id -> (oac,a)) where
    OnActionClick oac a = setOnActionClick oac a