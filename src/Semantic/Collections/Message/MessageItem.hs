module Semantic.Collections.Message.MessageItem where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data MessageItem ms = MessageItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageItem ms) where
    def = (G.to gdef) { as = Div }

pattern MessageItem :: Typeable ms => MessageItem ms -> View ms
pattern MessageItem mi = View mi

instance Typeable ms => Pure MessageItem ms where
    render MessageItem_ {..} =
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
