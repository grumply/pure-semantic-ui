module Semantic.Properties.FireOnMount where

import Semantic.Properties.Utils

class HasFireOnMountProp a where
    getFireOnMount :: a -> Bool
    setFireOnMount :: Bool -> a -> a

pattern FireOnMount :: HasFireOnMountProp a => a -> a
pattern FireOnMount a <- (getFireOnMount &&& id -> (True,a)) where
    FireOnMount a = setFireOnMount True a