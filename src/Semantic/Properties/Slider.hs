module Semantic.Properties.Slider where

import Semantic.Properties.Utils

class HasSliderProp a where
    getSlider :: a -> Bool
    setSlider :: Bool -> a -> a

pattern Slider :: HasSliderProp a => a -> a
pattern Slider a <- (getSlider &&& id -> (True,a)) where
    Slider a = setSlider True a