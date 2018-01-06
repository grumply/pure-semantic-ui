module Semantic.Properties.AspectRatio where

import Semantic.Properties.Utils

class HasAspectRatioProp a where
    getAspectRatio :: a -> Txt
    setAspectRatio :: Txt -> a -> a

pattern AspectRatio :: HasAspectRatioProp a => Txt -> a -> a
pattern AspectRatio ar a <- (getAspectRatio &&& id -> (ar,a)) where
    AspectRatio ar a = setAspectRatio ar a