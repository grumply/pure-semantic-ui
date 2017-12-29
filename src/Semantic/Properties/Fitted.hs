module Semantic.Properties.Fitted where

import Semantic.Properties.Utils

class HasFittedProp a where
    type FittedProp a :: *
    type FittedProp a = Bool
    getFitted :: a -> FittedProp a
    setFitted :: FittedProp a -> a -> a

pattern Fitted :: HasFittedProp a => FittedProp a -> a -> a
pattern Fitted f a <- (getFitted &&& id -> (f,a)) where
    Fitted f a = setFitted f a