module Semantic.Properties.TextAlign where

import Semantic.Properties.Utils

class HasTextAlignProp a where
    getTextAlign :: a -> Txt
    setTextAlign :: Txt -> a -> a

pattern TextAlign :: HasTextAlignProp a => Txt -> a -> a
pattern TextAlign ta a <- (getTextAlign &&& id -> (ta,a)) where
    TextAlign ta a = setTextAlign ta a
    
