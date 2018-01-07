module Semantic.Collections.Message.MessageContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data MessageContent ms = MessageContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageContent ms) where
    def = (G.to gdef) { as = Div }

pattern MessageContent :: MessageContent ms -> View ms
pattern MessageContent mc = View mc

instance Pure MessageContent ms where
    render MessageContent_ {..} =
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

instance HasAsProp (MessageContent ms) where
    type AsProp (MessageContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mc = mc { as = a }

instance HasAttributesProp (MessageContent ms) where
    type Attribute (MessageContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as mc = mc { attributes = as }

instance HasChildrenProp (MessageContent ms) where
    type Child (MessageContent ms) = View ms
    getChildren = children
    setChildren cs mc = mc { children = cs }

instance HasClassesProp (MessageContent ms) where
    getClasses = classes
    setClasses cs mc = mc { classes = cs }

