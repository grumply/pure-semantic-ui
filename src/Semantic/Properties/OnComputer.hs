module Semantic.Properties.OnComputer where

import Semantic.Utils
import Semantic.Properties.Utils

class HasOnComputerProp a where
    getOnComputer :: a -> Width
    setOnComputer :: Width -> a -> a

pattern OnComputer :: HasOnComputerProp a => Width -> a -> a
pattern OnComputer w a <- (getOnComputer &&& id -> (w,a)) where
    OnComputer w a = setOnComputer w a