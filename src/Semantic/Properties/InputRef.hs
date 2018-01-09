module Semantic.Properties.InputRef where

import Semantic.Properties.Utils

class HasInputRefProp a where
    type InputRefProp a
    getInputRef :: a -> InputRefProp a
    setInputRef :: InputRefProp a -> a -> a

pattern InputRef :: HasInputRefProp a => InputRefProp a -> a -> a
pattern InputRef ir a <- (getInputRef &&& id -> (ir,a)) where
    InputRef ir a = setInputRef ir a