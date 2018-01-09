module Semantic.Properties.ScrollContext where

import Pure.Lifted (JSV)
import Semantic.Properties.Utils

class HasScrollContextProp a where
    getScrollContext :: a -> Maybe JSV
    setScrollContext :: Maybe JSV -> a -> a

pattern ScrollContext :: HasScrollContextProp a => Maybe JSV -> a -> a
pattern ScrollContext sc a <- (getScrollContext &&& id -> (sc,a)) where
    ScrollContext sc a = setScrollContext sc a