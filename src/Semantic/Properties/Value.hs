module Semantic.Properties.Value where

import Semantic.Properties.Utils

class HasValueProp a where
    type ValueProp a :: *
    type ValueProp a = Txt
    getValue :: a -> ValueProp a
    setValue :: ValueProp a -> a -> a

pattern Value :: HasValueProp a => ValueProp a -> a -> a
pattern Value t a <- (getValue &&& id -> (t,a)) where
    Value t a = setValue t a