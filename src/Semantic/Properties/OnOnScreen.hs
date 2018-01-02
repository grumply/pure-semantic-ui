module Semantic.Properties.OnOnScreen where

import Semantic.Properties.Utils

class HasOnOnScreenProp a where
    type OnOnScreenProp a
    getOnOnScreen :: a -> OnOnScreenProp a
    setOnOnScreen :: OnOnScreenProp a -> a -> a

pattern OnOnScreen :: HasOnOnScreenProp a => OnOnScreenProp a -> a -> a
pattern OnOnScreen ou a <- (getOnOnScreen &&& id -> (ou,a)) where
    OnOnScreen ou a = setOnOnScreen ou a