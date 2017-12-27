module Semantic.Extensions.Animated where

import Semantic.Extensions.Utils

class HasAnimated a where
    type Anim a
    getAnimated :: a -> Anim a
    setAnimated :: Anim a -> a -> a

pattern Animated :: HasAnimated a => Anim a -> a -> a
pattern Animated anim a <- (getAnimated &&& id -> (anim,a)) where
    Animated anim a = setAnimated anim a
        
    
