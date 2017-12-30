module Semantic.Properties.Sorted where

import Semantic.Properties.Utils

class HasSortedProp a where
    getSorted :: a -> Txt
    setSorted :: Txt -> a -> a

pattern Sorted :: HasSortedProp a => Txt -> a -> a
pattern Sorted s a <- (getSorted &&& id -> (s,a)) where
    Sorted s a = setSorted s a