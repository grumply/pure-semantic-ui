module Semantic.Properties.CloseOnRootNodeClick where

import Semantic.Properties.Utils

class HasCloseOnRootNodeClickProp a where
    getCloseOnRootNodeClick :: a -> Bool
    setCloseOnRootNodeClick :: Bool -> a -> a

pattern CloseOnRootNodeClick :: HasCloseOnRootNodeClickProp a => a -> a
pattern CloseOnRootNodeClick a <- (getCloseOnRootNodeClick &&& id -> (True,a)) where
    CloseOnRootNodeClick a = setCloseOnRootNodeClick True a