module Semantic.Properties.Scrolling where

import Semantic.Properties.Utils

class HasScrollingProp a where
    getScrolling :: a -> Bool
    setScrolling :: Bool -> a -> a

pattern Scrolling :: HasScrollingProp a => a -> a
pattern Scrolling a <- (getScrolling &&& id -> (True,a)) where
    Scrolling a = setScrolling True a