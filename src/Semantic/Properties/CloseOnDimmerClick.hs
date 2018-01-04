module Semantic.Properties.CloseOnDimmerClick where

import Semantic.Properties.Utils

class HasCloseOnDimmerClickProp a where
    getCloseOnDimmerClick :: a -> Bool
    setCloseOnDimmerClick :: Bool -> a -> a

pattern CloseOnDimmerClick :: HasCloseOnDimmerClickProp a => a -> a
pattern CloseOnDimmerClick a <- (getCloseOnDimmerClick &&& id -> (True,a)) where
    CloseOnDimmerClick a = setCloseOnDimmerClick True a