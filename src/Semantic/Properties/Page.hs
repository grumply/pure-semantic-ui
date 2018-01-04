module Semantic.Properties.Page where

import Semantic.Properties.Utils

class HasPageProp a where
    getPage :: a -> Bool
    setPage :: Bool -> a -> a

pattern Page :: HasPageProp a => a -> a
pattern Page a <- (getPage &&& id -> (True,a)) where
    Page a = setPage True a