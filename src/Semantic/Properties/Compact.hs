module Semantic.Properties.Compact where

import Semantic.Properties.Utils

class HasCompactProp a where
    type CompactProp a :: *
    type CompactProp a = Bool
    getCompact :: a -> CompactProp a
    setCompact :: CompactProp a -> a -> a

pattern Compact :: HasCompactProp a => CompactProp a -> a -> a
pattern Compact c a <- (getCompact &&& id -> (c,a)) where
    Compact c a = setCompact c a