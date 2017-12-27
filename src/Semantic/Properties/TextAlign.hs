module Semantic.Properties.TextAlign where

import Semantic.Properties.Utils

class HasTextAlignProp a where
    getTextAlign :: a -> Txt
    setTextAlign :: Txt -> a -> a

pattern TextAlign :: HasTextAlignProp a => Txt -> a -> a
pattern TextAlign ta a <- (getTextAlign &&& id -> (ta,a)) where
    TextAlign ta a = setTextAlign ta a
    
pattern Unaligned c = TextAlign "" c
pattern LeftAligned c = TextAlign "left aligned" c
pattern RightAligned c = TextAlign "right aligned" c
pattern CenterAligned c = TextAlign "center aligned" c
pattern Justified c = TextAlign "justified" c

