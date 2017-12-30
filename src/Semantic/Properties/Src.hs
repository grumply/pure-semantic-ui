module Semantic.Properties.Src where

import Semantic.Properties.Utils

class HasSrcProp a where
    getSrc :: a -> Txt
    setSrc :: Txt -> a -> a

pattern Src :: HasSrcProp a => Txt -> a -> a
pattern Src s a <- (getSrc &&& id -> (s,a)) where
    Src s a = setSrc s a