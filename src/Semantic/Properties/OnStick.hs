module Semantic.Properties.OnStick where

import Semantic.Properties.Utils

class HasOnStickProp a where
    type OnStickProp a
    getOnStick :: a -> OnStickProp a
    setOnStick :: OnStickProp a -> a -> a

pattern OnStick :: HasOnStickProp a => OnStickProp a -> a -> a
pattern OnStick osp a <- (getOnStick &&& id -> (osp,a)) where
    OnStick osp a = setOnStick osp a