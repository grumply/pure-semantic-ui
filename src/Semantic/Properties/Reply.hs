module Semantic.Properties.Reply where

import Semantic.Properties.Utils

class HasReplyProp a where
    getReply :: a -> Bool
    setReply :: Bool -> a -> a

pattern Reply :: HasReplyProp a => a -> a
pattern Reply a <- (getReply &&& id -> (True,a)) where
    Reply a = setReply True a