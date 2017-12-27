module Semantic.Properties.Value where

import Semantic.Properties.Utils

class HasValueProp a where
    getValue :: a -> Txt
    setValue :: Txt -> a -> a

pattern Value :: HasValueProp a => Txt -> a -> a
pattern Value t a <- (getValue &&& id -> (t,a)) where
    Value t a = setValue t a