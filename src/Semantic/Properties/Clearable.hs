module Semantic.Properties.Clearable where

import Semantic.Properties.Utils

class HasClearableProp a where
    getClearable :: a -> Maybe Txt
    setClearable :: Maybe Txt -> a -> a

pattern Clearable :: HasClearableProp a => Maybe Txt -> a -> a
pattern Clearable c a <- (getClearable &&& id -> (c,a)) where
    Clearable c a = setClearable c a