module Semantic.Properties.Size where

import Semantic.Properties.Utils

class HasSizeProp a where
    getSize :: a -> Txt
    setSize :: Txt -> a -> a

pattern Size :: HasSizeProp a => Txt -> a -> a
pattern Size s a <- (getSize &&& id -> (s,a)) where
    Size s a = setSize s a

pattern Mini c = Size "mini" c
pattern Tiny c = Size "tiny" c
pattern Small c = Size "small" c
pattern Medium c = Size "medium" c
pattern Large c = Size "large" c
pattern Big c = Size "big" c
pattern Huge c = Size "huge" c
pattern Massive c = Size "massive" c

