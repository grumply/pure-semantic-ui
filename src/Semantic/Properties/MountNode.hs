module Semantic.Properties.MountNode where

import Pure.Lifted (JSV)
import Semantic.Properties.Utils

class HasMountNodeProp a where
    getMountNode :: a -> Maybe JSV
    setMountNode :: Maybe JSV -> a -> a

pattern MountNode :: HasMountNodeProp a => Maybe JSV -> a -> a
pattern MountNode mn a <- (getMountNode &&& id -> (mn,a)) where
    MountNode mn a = setMountNode mn a