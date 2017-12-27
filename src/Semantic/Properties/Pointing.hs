module Semantic.Properties.Pointing where

import Semantic.Properties.Utils

class HasPointingProp a where
    getPointing :: a -> Maybe Txt
    setPointing :: Maybe Txt -> a -> a

pattern Pointing :: HasPointingProp a => Maybe Txt -> a -> a
pattern Pointing p a <- (getPointing &&& id -> (p,a)) where
    Pointing p a = setPointing p a