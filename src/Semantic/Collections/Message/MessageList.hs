module Semantic.Collections.Message.MessageList where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

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

instance HasAsProp (MessageList ms) where
    type AsProp (MessageList ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ml = ml { as = a }

instance HasAttributesProp (MessageList ms) where
    type Attribute (MessageList ms) = Feature ms
    getAttributes = attributes
    setAttributes as ml = ml { attributes = as }

instance HasChildrenProp (MessageList ms) where
    type Child (MessageList ms) = View ms
    getChildren = children
    setChildren cs ml = ml { children = cs }

instance HasClassesProp (MessageList ms) where
    getClasses = classes
    setClasses cs ml = ml { classes = cs }