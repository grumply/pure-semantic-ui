module Semantic.Properties.OnOffScreen where

import Semantic.Properties.Utils

class HasOnOffScreenProp a where
    type OnOffScreenProp a
    getOnOffScreen :: a -> OnOffScreenProp a
    setOnOffScreen :: OnOffScreenProp a -> a -> a

pattern OnOffScreen :: HasOnOffScreenProp a => OnOffScreenProp a -> a -> a
pattern OnOffScreen ou a <- (getOnOffScreen &&& id -> (ou,a)) where
    OnOffScreen ou a = setOnOffScreen ou a