module Semantic.Properties.WithModal where

import Semantic.Properties.Utils

class HasWithModalProp a where
    type WithModalProp a
    getWithModal :: a -> WithModalProp a
    setWithModal :: WithModalProp a -> a -> a

pattern WithModal :: HasWithModalProp a => WithModalProp a -> a -> a
pattern WithModal wm a <- (getWithModal &&& id -> (wm,a)) where
    WithModal wm a = setWithModal wm a