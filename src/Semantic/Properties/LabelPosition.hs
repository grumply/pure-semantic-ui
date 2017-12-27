module Semantic.Properties.LabelPosition where

import Semantic.Properties.Utils

class HasLabelPositionProp a where
    getLabelPosition :: a -> Txt
    setLabelPosition :: Txt -> a -> a

pattern LabelPosition :: HasLabelPositionProp a => Txt -> a -> a
pattern LabelPosition lp a <- (getLabelPosition &&& id -> (lp,a)) where
    LabelPosition lp a = setLabelPosition lp a