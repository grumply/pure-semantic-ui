module Semantic.Properties.Floated where

import Semantic.Properties.Utils

class HasFloatedProp a where
    type FloatedProp a :: *
    type FloatedProp a = Txt
    getFloated :: a -> FloatedProp a
    setFloated :: FloatedProp a -> a -> a

pattern Floated :: HasFloatedProp a => FloatedProp a -> a -> a
pattern Floated f a <- (getFloated &&& id -> (f,a)) where
    Floated f a = setFloated f a