module Semantic.Properties.Floated where

import Semantic.Properties.Utils

class HasFloatedProp a where
    getFloated :: a -> Txt
    setFloated :: Txt -> a -> a

pattern Floated :: HasFloatedProp a => Txt -> a -> a
pattern Floated f a <- (getFloated &&& id -> (f,a)) where
    Floated f a = setFloated f a