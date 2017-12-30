module Semantic.Properties.Threaded where

import Semantic.Properties.Utils

class HasThreadedProp a where
    getThreaded :: a -> Bool
    setThreaded :: Bool -> a -> a

pattern Threaded :: HasThreadedProp a => a -> a
pattern Threaded a <- (getThreaded &&& id -> (True,a)) where
    Threaded a = setThreaded True a