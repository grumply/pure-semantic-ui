module Semantic.Properties.URL where

import Semantic.Properties.Utils

class HasURLProp a where
    getURL :: a -> Txt
    setURL :: Txt -> a -> a

pattern URL :: HasURLProp a => Txt -> a -> a
pattern URL u a <- (getURL &&& id -> (u,a)) where
    URL u a = setURL u a