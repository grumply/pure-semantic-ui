module Semantic.Properties.Ref where

import Semantic.Properties.Utils

class HasRefProp a where
    type RefProp a
    getRef :: a -> RefProp a
    setRef :: RefProp a -> a -> a
    
pattern Ref :: HasRefProp a => RefProp a -> a -> a
pattern Ref r a <- (getRef &&& id -> (r,a)) where
    Ref r a = setRef r a