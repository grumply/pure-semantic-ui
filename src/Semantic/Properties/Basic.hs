module Semantic.Properties.Basic where

import Semantic.Properties.Utils

class HasBasicProp a where
    type BasicProp a :: *
    type BasicProp a = Bool
    getBasic :: a -> BasicProp a
    setBasic :: BasicProp a -> a -> a

pattern Basic :: HasBasicProp a => BasicProp a -> a -> a
pattern Basic b a <- (getBasic &&& id -> (b,a)) where
    Basic b a = setBasic b a