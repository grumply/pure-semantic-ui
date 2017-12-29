module Semantic.Collections.Message.MessageHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data MessageHeader ms = MessageHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageHeader ms) where
    def = (G.to gdef) { as = Div }

pattern MessageHeader :: Typeable ms => MessageHeader ms -> View ms
pattern MessageHeader mc = View mc

instance Typeable ms => Pure MessageHeader ms where
    render MessageHeader_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
