module Semantic.Properties.Aligned where

import Semantic.Properties.Utils

class HasAlignedProp a where
    getAligned :: a -> Txt
    setAligned :: Txt -> a -> a

pattern Aligned :: HasAlignedProp a => Txt -> a -> a
pattern Aligned ad a <- (getAligned &&& id -> (ad,a)) where
    Aligned ad a = setAligned ad a