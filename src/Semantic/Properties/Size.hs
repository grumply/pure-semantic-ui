module Semantic.Properties.Size where

import Semantic.Properties.Utils

class HasSizeProp a where
    getSize :: a -> Txt
    setSize :: Txt -> a -> a

pattern Size :: HasSizeProp a => Txt -> a -> a
pattern Size s a <- (getSize &&& id -> (s,a)) where
    Size s a = setSize s a
