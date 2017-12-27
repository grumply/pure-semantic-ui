module Semantic.Properties.Link where

import Semantic.Properties.Utils

class HasLinkProp a where
    getLink :: a -> Bool
    setLink :: Bool -> a -> a

pattern Link :: HasLinkProp a => a -> a
pattern Link a <- (getLink &&& id -> (True,a)) where
    Link a = setLink True a