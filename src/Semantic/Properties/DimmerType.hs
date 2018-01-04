module Semantic.Properties.DimmerType where

import Semantic.Properties.Utils

class HasDimmerTypeProp a where
    getDimmerType :: a -> Maybe Txt
    setDimmerType :: Maybe Txt -> a -> a

pattern DimmerType :: HasDimmerTypeProp a => Maybe Txt -> a -> a
pattern DimmerType b a <- (getDimmerType &&& id -> (b,a)) where
    DimmerType b a = setDimmerType b a