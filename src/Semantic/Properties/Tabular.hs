module Semantic.Properties.Tabular where

import Semantic.Properties.Utils

class HasTabularProp a where
    getTabular :: a -> Maybe Txt
    setTabular :: Maybe Txt -> a -> a

pattern Tabular :: HasTabularProp a => Maybe Txt -> a -> a
pattern Tabular i a <- (getTabular &&& id -> (i,a)) where
    Tabular i a = setTabular i a