module Semantic.Properties.UnmountOnHide where

import Semantic.Properties.Utils

class HasUnmountOnHideProp a where
    getUnmountOnHide :: a -> Bool
    setUnmountOnHide :: Bool -> a -> a

pattern UnmountOnHide :: HasUnmountOnHideProp a => a -> a
pattern UnmountOnHide a <- (getUnmountOnHide &&& id -> (True,a)) where
    UnmountOnHide a = setUnmountOnHide True a