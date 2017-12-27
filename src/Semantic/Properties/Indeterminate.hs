module Semantic.Properties.Indeterminate where

import Semantic.Properties.Utils

class HasIndeterminateProp a where
    getIndeterminate :: a -> Bool
    setIndeterminate :: Bool -> a -> a

pattern Indeterminate :: HasIndeterminateProp a => a -> a
pattern Indeterminate a <- (getIndeterminate &&& id -> (True,a)) where
    Indeterminate a = setIndeterminate True a