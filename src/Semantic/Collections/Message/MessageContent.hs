module Semantic.Collections.Message.MessageContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data MessageContent ms = MessageContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageContent ms) where
    def = (G.to gdef) { as = Div }

pattern MessageContent :: Typeable ms => MessageContent ms -> View ms
pattern MessageContent mc = View mc

instance Typeable ms => Pure MessageContent ms where
    render MessageContent_ {..} =
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