module Semantic.Properties.Pagination where

import Semantic.Properties.Utils

class HasPaginationProp a where
    getPagination :: a -> Bool
    setPagination :: Bool -> a -> a

pattern Pagination :: HasPaginationProp a => a -> a
pattern Pagination a <- (getPagination &&& id -> (True,a)) where
    Pagination a = setPagination True a