module Semantic.Properties.Classes where

import Pure.View

import Semantic.Properties.Utils

class HasClassesProp a where
    getClasses :: a -> [Txt]
    setClasses :: [Txt] -> a -> a

pattern Classes :: HasClassesProp a => [Txt] -> a -> a
pattern Classes cs a <- (getClasses &&& id -> (cs,a)) where
    Classes cs a = setClasses (getClasses a ++ cs) a
