module Semantic.Properties.Test where

import Semantic.Properties.Utils

class HasTestProp a where
    getTest :: a -> Txt
    setTest :: Txt -> a -> a

pattern Test :: HasTestProp a => Txt -> a -> a
pattern Test t a <- (getTest &&& id -> (t,a)) where
    Test t a = setTest t a