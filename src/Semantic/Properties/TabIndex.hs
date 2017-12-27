module Semantic.Properties.TabIndex where

import Semantic.Properties.Utils

class HasTabIndexProp a where
    getTabIndex :: a -> Maybe Int
    setTabIndex :: Maybe Int -> a -> a

pattern TabIndex :: HasTabIndexProp a => Maybe Int -> a -> a
pattern TabIndex n a <- (getTabIndex &&& id -> (n,a)) where
    TabIndex n a = setTabIndex n a