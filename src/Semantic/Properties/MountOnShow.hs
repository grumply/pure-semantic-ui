module Semantic.Properties.MountOnShow where

import Semantic.Properties.Utils

class HasMountOnShowProp a where
    getMountOnShow :: a -> Bool
    setMountOnShow :: Bool -> a -> a

pattern MountOnShow :: HasMountOnShowProp a => a -> a
pattern MountOnShow a <- (getMountOnShow &&& id -> (True,a)) where
    MountOnShow a = setMountOnShow True a