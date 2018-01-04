module Semantic.Properties.AutoSuccess where

import Semantic.Properties.Utils

class HasAutoSuccessProp a where
    getAutoSuccess :: a -> Bool
    setAutoSuccess :: Bool -> a -> a

pattern AutoSuccess :: HasAutoSuccessProp a => a -> a
pattern AutoSuccess a <- (getAutoSuccess &&& id -> (True,a)) where
    AutoSuccess a = setAutoSuccess True a