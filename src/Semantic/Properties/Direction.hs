module Semantic.Properties.Direction where

import GHC.Generics as G
import Pure.View

import Semantic.Properties.Utils

class HasDirectionProp a where
    getDirection :: a -> Txt
    setDirection :: Txt -> a -> a

pattern Direction :: HasDirectionProp a => Txt -> a -> a
pattern Direction d a <- (getDirection &&& id -> (d,a)) where
    Direction d a = setDirection d a