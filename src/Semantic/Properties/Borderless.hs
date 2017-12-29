module Semantic.Properties.Borderless where

import Semantic.Properties.Utils

class HasBorderlessProp a where
    getBorderless :: a -> Bool
    setBorderless :: Bool -> a -> a

pattern Borderless :: HasBorderlessProp a => a -> a
pattern Borderless a <- (getBorderless &&& id -> (True,a)) where
    Borderless a = setBorderless True a