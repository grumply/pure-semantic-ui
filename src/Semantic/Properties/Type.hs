module Semantic.Properties.Type where

import Semantic.Properties.Utils

class HasTypeProp a where
    getType :: a -> Txt
    setType :: Txt -> a -> a

pattern Type :: HasTypeProp a => Txt -> a -> a
pattern Type t a <- (getType &&& id -> (t,a)) where
    Type t a = setType t a