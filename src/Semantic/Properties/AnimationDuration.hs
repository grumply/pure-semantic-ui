module Semantic.Properties.AnimationDuration where

import Semantic.Properties.Utils

data AnimationDuration
    = Uniform Int
    | Skewed 
        { hide :: Int 
        , show :: Int
        }
    deriving (Generic,Default,Ord,Eq)

class HasAnimationDurationProp a where
    getAnimationDuration :: a -> AnimationDuration
    setAnimationDuration :: AnimationDuration -> a -> a

pattern AnimationDuration :: HasAnimationDurationProp a => AnimationDuration -> a -> a
pattern AnimationDuration dur a <- (getAnimationDuration &&& id -> (dur,a)) where
    AnimationDuration dur a = setAnimationDuration dur a