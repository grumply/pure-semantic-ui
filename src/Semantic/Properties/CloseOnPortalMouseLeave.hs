module Semantic.Properties.CloseOnPortalMouseLeave where

import Semantic.Properties.Utils

class HasCloseOnPortalMouseLeaveProp a where
    getCloseOnPortalMouseLeave :: a -> Bool
    setCloseOnPortalMouseLeave :: Bool -> a -> a

pattern CloseOnPortalMouseLeave :: HasCloseOnPortalMouseLeaveProp a => a -> a
pattern CloseOnPortalMouseLeave a <- (getCloseOnPortalMouseLeave &&& id -> (True,a)) where
    CloseOnPortalMouseLeave a = setCloseOnPortalMouseLeave True a