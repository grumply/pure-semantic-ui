module Semantic.Properties.Placeholder where

import Semantic.Properties.Utils

class HasPlaceholderProp a where
    getPlaceholder :: a -> Txt
    setPlaceholder :: Txt -> a -> a

pattern Placeholder :: HasPlaceholderProp a => Txt -> a -> a
pattern Placeholder ph a <- (getPlaceholder &&& id -> (ph,a)) where
    Placeholder ph a = setPlaceholder ph a