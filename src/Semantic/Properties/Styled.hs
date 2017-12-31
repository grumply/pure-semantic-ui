module Semantic.Properties.Styled where

import Semantic.Properties.Utils

class HasStyledProp a where
    getStyled :: a -> Bool
    setStyled :: Bool -> a -> a

pattern Styled :: HasStyledProp a => a -> a
pattern Styled a <- (getStyled &&& id -> (True,a)) where
    Styled a = setStyled True a