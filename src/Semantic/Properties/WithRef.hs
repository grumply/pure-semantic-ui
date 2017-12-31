module Semantic.Properties.WithRef where

import Semantic.Properties.Utils

class HasWithRefProp a where
    type WithRefProp a
    getWithRef :: a -> WithRefProp a
    setWithRef :: WithRefProp a -> a -> a

pattern WithRef :: HasWithRefProp a => WithRefProp a -> a -> a
pattern WithRef wr a <- (getWithRef &&& id -> (wr,a)) where
    WithRef wr a = setWithRef wr a