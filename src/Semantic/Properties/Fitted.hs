module Semantic.Properties.Fitted where

import Semantic.Properties.Utils

class HasFittedProp a where
    getFitted :: a -> Bool
    setFitted :: Bool -> a -> a

pattern Fitted :: HasFittedProp a => a -> a
pattern Fitted a <- (getFitted &&& id -> (True,a)) where
    Fitted a = setFitted True a