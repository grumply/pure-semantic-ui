module Semantic.Properties.TriggerOn where

import Semantic.Properties.Utils

class HasTriggerOnProp a where
    getTriggerOn :: a -> [Txt]
    setTriggerOn :: [Txt] -> a -> a

pattern TriggerOn :: HasTriggerOnProp a => [Txt] -> a -> a
pattern TriggerOn ts a <- (getTriggerOn &&& id -> (ts,a)) where
    TriggerOn ts a = setTriggerOn ts a