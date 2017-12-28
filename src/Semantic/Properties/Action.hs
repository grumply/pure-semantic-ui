module Semantic.Properties.Action where

import Semantic.Properties.Utils

class HasActionProp a where
    getAction :: a -> Txt
    setAction :: Txt -> a -> a

pattern Action :: HasActionProp a => Txt -> a -> a
pattern Action t a <- (getAction &&& id -> (t,a)) where
    Action t a = setAction t a