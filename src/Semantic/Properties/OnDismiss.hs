module Semantic.Properties.OnDismiss where

import Semantic.Properties.Utils

class HasOnDismissProp a where
    type OnDismissProp a
    getOnDismiss :: a -> OnDismissProp a
    setOnDismiss :: OnDismissProp a -> a -> a

pattern OnDismiss :: HasOnDismissProp a => OnDismissProp a -> a -> a
pattern OnDismiss od a <- (getOnDismiss &&& id -> (od,a)) where
    OnDismiss od a = setOnDismiss od a