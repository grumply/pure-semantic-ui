module Semantic.Extensions.Attached where

import Semantic.Extensions.Utils

class HasAttached a where
    type Attach a
    getAttached :: a -> Attach a
    setAttached :: Attach a -> a -> a

pattern Attached :: HasAttached a => Attach a -> a -> a
pattern Attached att a <- (getAttached &&& id -> (att,a)) where
    Attached att a = setAttached att a