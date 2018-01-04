module Semantic.Properties.IsImage where

import Semantic.Properties.Utils

class HasIsImageProp a where
    getIsImage :: a -> Bool
    setIsImage :: Bool -> a -> a

pattern IsImage :: HasIsImageProp a => a -> a
pattern IsImage a <- (getIsImage &&& id -> (True,a)) where
    IsImage a = setIsImage True a