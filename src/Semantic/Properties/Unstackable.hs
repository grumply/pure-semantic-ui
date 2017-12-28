module Semantic.Properties.Unstackable where

import Semantic.Properties.Utils

class HasUnstackableProp a where
    getUnstackable :: a -> Bool
    setUnstackable :: Bool -> a -> a

pattern Unstackable :: HasUnstackableProp a => a -> a
pattern Unstackable a <- (getUnstackable &&& id -> (True,a)) where
    Unstackable a = setUnstackable True a