module Semantic.Properties.Sortable where

import Semantic.Properties.Utils

class HasSortableProp a where
    getSortable :: a -> Bool
    setSortable :: Bool -> a -> a

pattern Sortable :: HasSortableProp a => a -> a
pattern Sortable a <- (getSortable &&& id -> (True,a)) where
    Sortable a = setSortable True a