module Semantic.Properties.HideOnScroll where

import Semantic.Properties.Utils

class HasHideOnScrollProp a where
    getHideOnScroll :: a -> Bool
    setHideOnScroll :: Bool -> a -> a

pattern HideOnScroll :: HasHideOnScrollProp a => a -> a
pattern HideOnScroll a <- (getHideOnScroll &&& id -> (True,a)) where
    HideOnScroll a = setHideOnScroll True a