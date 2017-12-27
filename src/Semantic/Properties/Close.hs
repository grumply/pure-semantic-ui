module Semantic.Properties.Close where

import Semantic.Properties.Utils

class HasCloseProp a where
    getClose :: a -> Maybe Txt
    setClose :: Maybe Txt -> a -> a

pattern Close :: HasCloseProp a => Maybe Txt -> a -> a
pattern Close c a <- (getClose &&& id -> (c,a)) where
    Close c a = setClose c a