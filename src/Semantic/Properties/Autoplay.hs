module Semantic.Properties.Autoplay where

import Semantic.Properties.Utils

class HasAutoplayProp a where
    getAutoplay :: a -> Bool
    setAutoplay :: Bool -> a -> a

pattern Autoplay :: HasAutoplayProp a => a -> a
pattern Autoplay a <- (getAutoplay &&& id -> (True,a)) where
    Autoplay a = setAutoplay True a
        
