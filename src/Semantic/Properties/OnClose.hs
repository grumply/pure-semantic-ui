module Semantic.Properties.OnClose where

import Semantic.Properties.Utils

class HasOnCloseProp a where
    type OnCloseProp a
    getOnClose :: a -> OnCloseProp a
    setOnClose :: OnCloseProp a -> a -> a

pattern OnClose :: HasOnCloseProp a => OnCloseProp a -> a -> a
pattern OnClose oc a <- (getOnClose &&& id -> (oc,a)) where
    OnClose oc a = setOnClose oc a