module Semantic.Collections.Message.MessageItem where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data MessageItem ms = MessageItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageItem ms) where
    def = (G.to gdef) { as = Li }

pattern MessageItem :: MessageItem ms -> View ms
pattern MessageItem mi = View mi

instance Pure MessageItem ms where
    render MessageItem_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (MessageItem ms) where
    type AsProp (MessageItem ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mi = mi { as = a }

instance HasAttributesProp (MessageItem ms) where
    type Attribute (MessageItem ms) = Feature ms
    getAttributes = attributes
    setAttributes as mi = mi { attributes = as }

instance HasChildrenProp (MessageItem ms) where
    type Child (MessageItem ms) = View ms
    getChildren = children
    setChildren cs mi = mi { children = cs }

instance HasClassesProp (MessageItem ms) where
    getClasses = classes
    setClasses cs mi = mi { classes = cs }

