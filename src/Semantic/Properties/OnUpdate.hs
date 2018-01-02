module Semantic.Properties.OnUpdate where

import Semantic.Properties.Utils

class HasOnUpdateProp a where
    type OnUpdateProp a
    getOnUpdate :: a -> OnUpdateProp a
    setOnUpdate :: OnUpdateProp a -> a -> a

pattern OnUpdate :: HasOnUpdateProp a => OnUpdateProp a -> a -> a
pattern OnUpdate ou a <- (getOnUpdate &&& id -> (ou,a)) where
    OnUpdate ou a = setOnUpdate ou a