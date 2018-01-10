module Semantic.Properties.IsSearch where

import Semantic.Properties.Utils

class HasIsSearchProp a where
    getIsSearch :: a -> Bool
    setIsSearch :: Bool -> a -> a

pattern IsSearch :: HasIsSearchProp a => a -> a
pattern IsSearch a <- (getIsSearch &&& id -> (True,a)) where
    IsSearch a = setIsSearch True a