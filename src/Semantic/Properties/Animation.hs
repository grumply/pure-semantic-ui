module Semantic.Properties.Animation where

import Semantic.Properties.Utils

class HasAnimationProp a where
    getAnimation :: a -> Txt
    setAnimation :: Txt -> a -> a

pattern Animation :: HasAnimationProp a => Txt -> a -> a
pattern Animation anim a <- (getAnimation &&& id -> (anim,a)) where
    Animation anim a = setAnimation anim a