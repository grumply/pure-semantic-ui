module Semantic.Properties.OnRate where

import Semantic.Properties.Utils

class HasOnRateProp a where
    type OnRateProp a
    getOnRate :: a -> OnRateProp a
    setOnRate :: OnRateProp a -> a -> a

pattern OnRate :: HasOnRateProp a => OnRateProp a -> a -> a
pattern OnRate or a <- (getOnRate &&& id -> (or,a)) where
    OnRate or a = setOnRate or a