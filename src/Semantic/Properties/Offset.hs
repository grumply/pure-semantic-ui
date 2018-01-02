module Semantic.Properties.Offset where

import Semantic.Properties.Utils

class HasOffsetProp a where
    type OffsetProp a
    getOffset :: a -> OffsetProp a
    setOffset :: OffsetProp a -> a -> a
    
pattern Offset :: HasOffsetProp a => OffsetProp a -> a -> a
pattern Offset op a <- (getOffset &&& id -> (op,a)) where
    Offset op a = setOffset op a
