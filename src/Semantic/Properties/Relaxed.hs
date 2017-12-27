module Semantic.Properties.Relaxed where

import Semantic.Properties.Utils

class HasRelaxedProp a where
    getRelaxed :: a -> Maybe Txt
    setRelaxed :: Maybe Txt -> a -> a

pattern Relaxed :: HasRelaxedProp a => Maybe Txt -> a -> a
pattern Relaxed r a <- (getRelaxed &&& id -> (r,a)) where
    Relaxed r a = setRelaxed r a