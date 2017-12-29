module Semantic.Collections.Message.MessageList where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data MessageList ms = MessageList_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageList ms) where
    def = (G.to gdef) { as = Ul }

pattern MessageList :: Typeable ms => MessageList ms -> View ms
pattern MessageList ml = View ml 

instance Typeable ms => Pure MessageList ms where
    render MessageList_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
