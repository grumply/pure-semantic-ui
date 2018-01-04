module Semantic.Properties.Styles where

import Semantic.Properties.Utils

class HasStylesProp a where
    getStyles :: a -> [(Txt,Txt)]
    setStyles :: [(Txt,Txt)] -> a -> a

pattern Styles :: HasStylesProp a => [(Txt,Txt)] -> a -> a
pattern Styles ss a <- (getStyles &&& id -> (ss,a)) where
    Styles ss a = setStyles ss a