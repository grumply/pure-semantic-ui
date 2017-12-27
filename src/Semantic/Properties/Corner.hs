module Semantic.Properties.Corner where

import Semantic.Properties.Utils

class HasCornerProp a where
    type CornerProp a
    getCorner :: a -> CornerProp a
    setCorner :: CornerProp a -> a -> a

pattern Corner :: HasCornerProp a => CornerProp a -> a -> a
pattern Corner c a <- (getCorner &&& id -> (c,a)) where
    Corner c a = setCorner c a