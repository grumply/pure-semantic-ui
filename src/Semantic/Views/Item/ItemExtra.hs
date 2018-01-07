module Semantic.Views.Item.ItemExtra where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ItemExtra ms = ItemExtra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemExtra ms) where
    def = (G.to gdef) { as = Div }

pattern ItemExtra :: ItemExtra ms -> View ms
pattern ItemExtra ie = View ie

instance Pure ItemExtra ms where
    render ItemExtra_ {..} =
        let
            cs =
                ( "extra"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemExtra ms) where
    type AsProp (ItemExtra ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ie = ie { as = a }

instance HasAttributesProp (ItemExtra ms) where
    type Attribute (ItemExtra ms) = Feature ms
    getAttributes = attributes
    setAttributes as ie = ie { attributes = as }

instance HasChildrenProp (ItemExtra ms) where
    type Child (ItemExtra ms) = View ms
    getChildren = children
    setChildren cs ie = ie { children = cs }

instance HasClassesProp (ItemExtra ms) where
    getClasses = classes
    setClasses cs ie = ie { classes = cs }
