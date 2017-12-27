module Semantic.Properties.Padded where

import Semantic.Properties.Utils

class HasPaddedProp a where
    getPadded :: a -> Maybe Txt
    setPadded :: Maybe Txt -> a -> a

pattern Padded :: HasPaddedProp a => Maybe Txt -> a -> a
pattern Padded p a <- (getPadded &&& id -> (p,a)) where
    Padded p a = setPadded p a