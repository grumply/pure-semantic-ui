module Semantic.Properties.ItemsPerRow where

import Semantic.Utils
import Semantic.Properties.Utils

class HasItemsPerRowProp a where
    getItemsPerRow :: a -> Width
    setItemsPerRow :: Width -> a -> a

pattern ItemsPerRow :: HasItemsPerRowProp a => Width -> a -> a
pattern ItemsPerRow w a <- (getItemsPerRow &&& id -> (w,a)) where
    ItemsPerRow w a = setItemsPerRow w a