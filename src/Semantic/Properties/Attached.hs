module Semantic.Properties.Attached where

import Semantic.Properties.Utils

class HasAttachedProp a where
    type AttachedProp a
    getAttached :: a -> AttachedProp a
    setAttached :: AttachedProp a -> a -> a

pattern Attached :: HasAttachedProp a => AttachedProp a -> a -> a
pattern Attached att a <- (getAttached &&& id -> (att,a)) where
    Attached att a = setAttached att a