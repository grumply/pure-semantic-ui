module Semantic.Properties.Bulleted where

import Semantic.Properties.Utils

class HasBulletedProp a where
    getBulleted :: a -> Bool
    setBulleted :: Bool -> a -> a

pattern Bulleted :: HasBulletedProp a => a -> a
pattern Bulleted a <- (getBulleted &&& id -> (True,a)) where
    Bulleted a = setBulleted True a