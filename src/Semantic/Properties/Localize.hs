module Semantic.Properties.Localize where

import Semantic.Properties.Utils

class HasLocalizeProp a where
    getLocalize :: a -> Txt
    setLocalize :: Txt -> a -> a

pattern Localize :: HasLocalizeProp a => Txt -> a -> a
pattern Localize l a <- (getLocalize &&& id -> (l,a)) where
    Localize l a = setLocalize l a