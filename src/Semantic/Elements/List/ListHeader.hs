module Semantic.Elements.List.ListHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ListHeader ms = ListHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ListHeader :: Typeable ms => ListHeader ms -> View ms
pattern ListHeader lh = View lh

instance Typeable ms => Pure ListHeader ms where
    render ListHeader_ {..} =
        as ( ClassList ( "header" : classes ) : attributes ) children

instance HasAsProp (ListHeader ms) where
    type AsProp (ListHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lh = lh { as = f }

instance HasAttributesProp (ListHeader ms) where
    type Attribute (ListHeader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs lh = lh { attributes = cs }

instance HasChildrenProp (ListHeader ms) where
    type Child (ListHeader ms) = View ms
    getChildren = children
    setChildren cs lh = lh { children = cs }

instance HasClassesProp (ListHeader ms) where
    getClasses = classes
    setClasses cs lh = lh { classes = cs }