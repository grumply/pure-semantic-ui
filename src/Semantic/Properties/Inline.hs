module Semantic.Properties.Inline where

import Semantic.Properties.Utils

class HasInlineProp a where
    type InlineProp a
    getInline :: a -> InlineProp a
    setInline :: InlineProp a -> a -> a

pattern Inline :: HasInlineProp a => InlineProp a -> a -> a
pattern Inline i a <- (getInline &&& id -> (i,a)) where
    Inline i a = setInline i a