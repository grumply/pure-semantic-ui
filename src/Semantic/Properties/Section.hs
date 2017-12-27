module Semantic.Properties.Section where

import Semantic.Properties.Utils

class HasSectionProp a where
    getSection :: a -> Bool
    setSection :: Bool -> a -> a

pattern Section :: HasSectionProp a => a -> a
pattern Section a <- (getSection &&& id -> (True,a)) where
    Section a = setSection True a