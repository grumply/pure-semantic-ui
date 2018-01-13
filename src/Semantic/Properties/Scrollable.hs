module Semantic.Properties.Scrollable where

import Semantic.Properties.Utils

class HasScrollableProp a where
    getScrollable :: a -> Bool
    setScrollable :: Bool -> a -> a

pattern Scrollable :: HasScrollableProp a => a -> a
pattern Scrollable a <- (getScrollable &&& id -> (True,a)) where
    Scrollable a = setScrollable True a