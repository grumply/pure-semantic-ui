module Semantic.Views.Item.ItemHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ItemHeader ms = ItemHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ItemHeader :: ItemHeader ms -> View ms
pattern ItemHeader ih = View ih

instance Pure ItemHeader ms where
    render ItemHeader_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemHeader ms) where
    type AsProp (ItemHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ih = ih { as = a }

instance HasAttributesProp (ItemHeader ms) where
    type Attribute (ItemHeader ms) = Feature ms
    getAttributes = attributes
    setAttributes as ih = ih { attributes = as }

instance HasChildrenProp (ItemHeader ms) where
    type Child (ItemHeader ms) = View ms
    getChildren = children
    setChildren cs ih = ih { children = cs }

instance HasClassesProp (ItemHeader ms) where
    getClasses = classes
    setClasses cs ih = ih { classes = cs }
