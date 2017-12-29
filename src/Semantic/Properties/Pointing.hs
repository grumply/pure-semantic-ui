module Semantic.Properties.Pointing where

import Semantic.Properties.Utils

class HasPointingProp a where
    type PointingProp a :: *
    type PointingProp a = Maybe Txt
    getPointing :: a -> PointingProp a
    setPointing :: PointingProp a -> a -> a

pattern Pointing :: HasPointingProp a => PointingProp a -> a -> a
pattern Pointing p a <- (getPointing &&& id -> (p,a)) where
    Pointing p a = setPointing p a