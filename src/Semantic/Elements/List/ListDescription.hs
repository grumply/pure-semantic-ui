module Semantic.Elements.List.ListDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ListDescription ms = ListDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ListDescription :: ListDescription ms -> View ms
pattern ListDescription ld = View ld

instance Pure ListDescription ms where
    render ListDescription_ {..} =
        as ( ClassList (classes ++ [ "description" ]) : attributes ) children

instance HasAsProp (ListDescription ms) where
    type AsProp (ListDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ld = ld { as = f }

instance HasAttributesProp (ListDescription ms) where
    type Attribute (ListDescription ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ld = ld { attributes = cs }

instance HasChildrenProp (ListDescription ms) where
    type Child (ListDescription ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }

instance HasClassesProp (ListDescription ms) where
    getClasses = classes
    setClasses cs ld = ld { classes = cs }