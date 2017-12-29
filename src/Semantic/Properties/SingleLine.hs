module Semantic.Properties.SingleLine where

import Semantic.Properties.Utils

class HasSingleLineProp a where
    getSingleLine :: a -> Bool
    setSingleLine :: Bool -> a -> a

pattern SingleLine :: HasSingleLineProp a => a -> a
pattern SingleLine a <- (getSingleLine &&& id -> (True,a)) where
    SingleLine a = setSingleLine True a