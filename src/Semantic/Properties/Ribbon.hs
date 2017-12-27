module Semantic.Properties.Ribbon where

import Semantic.Properties.Utils

class HasRibbonProp a where
    getRibbon :: a -> Maybe Txt
    setRibbon :: Maybe Txt -> a -> a

pattern Ribbon :: HasRibbonProp a => Maybe Txt -> a -> a
pattern Ribbon r a <- (getRibbon &&& id -> (r,a)) where
    Ribbon r a = setRibbon r a