module Semantic.Properties.Animated where

import Semantic.Properties.Utils

class HasAnimatedProp a where
    type AnimatedProp a
    getAnimated :: a -> AnimatedProp a
    setAnimated :: AnimatedProp a -> a -> a

pattern Animated :: HasAnimatedProp a => AnimatedProp a -> a -> a
pattern Animated anim a <- (getAnimated &&& id -> (anim,a)) where
    Animated anim a = setAnimated anim a
        
    
