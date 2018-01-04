module Semantic.Properties.WithPortal where

import Semantic.Properties.Utils

class HasWithPortalProp a where
    type WithPortalProp a
    getWithPortal :: a -> WithPortalProp a
    setWithPortal :: WithPortalProp a -> a -> a

pattern WithPortal :: HasWithPortalProp a => WithPortalProp a -> a -> a
pattern WithPortal wp a <- (getWithPortal &&& id -> (wp,a)) where
    WithPortal wp a = setWithPortal wp a