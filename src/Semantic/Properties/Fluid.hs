module Semantic.Properties.Fluid where

import Semantic.Properties.Utils

class HasFluidProp a where
    getFluid :: a -> Bool
    setFluid :: Bool -> a -> a

pattern Fluid :: HasFluidProp a => a -> a
pattern Fluid a <- (getFluid &&& id -> (True,a)) where
    Fluid a = setFluid True a