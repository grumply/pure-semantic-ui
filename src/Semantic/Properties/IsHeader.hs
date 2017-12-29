module Semantic.Properties.IsHeader where

import Semantic.Properties.Utils

class HasIsHeaderProp a where
    getIsHeader :: a -> Bool
    setIsHeader :: Bool -> a -> a

pattern IsHeader :: HasIsHeaderProp a => a -> a
pattern IsHeader a <- (getIsHeader &&& id -> (True,a)) where
    IsHeader a = setIsHeader True a