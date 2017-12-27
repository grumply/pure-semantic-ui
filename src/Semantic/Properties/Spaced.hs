module Semantic.Properties.Spaced where

import Semantic.Properties.Utils

class HasSpacedProp a where
    getSpaced :: a -> Maybe Txt
    setSpaced :: Maybe Txt -> a -> a

pattern Spaced :: HasSpacedProp a => Maybe Txt -> a -> a
pattern Spaced s a <- (getSpaced &&& id -> (s,a)) where
    Spaced s a = setSpaced s a