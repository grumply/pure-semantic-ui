module Semantic.Properties.Blurring where

import Semantic.Properties.Utils

class HasBlurringProp a where
    getBlurring :: a -> Bool
    setBlurring :: Bool -> a -> a

pattern Blurring :: HasBlurringProp a => a -> a
pattern Blurring a <- (getBlurring &&& id -> (True,a)) where
    Blurring a = setBlurring True a