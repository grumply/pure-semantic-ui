module Semantic.Properties.OnChange where

import Semantic.Properties.Utils

class HasOnChangeProp a where
    type OnChangeProp a
    getOnChange :: a -> OnChangeProp a 
    setOnChange :: OnChangeProp a -> a -> a

pattern OnChange :: HasOnChangeProp a => OnChangeProp a -> a -> a
pattern OnChange f a <- (getOnChange &&& id -> (f,a)) where
    OnChange f a = setOnChange f a