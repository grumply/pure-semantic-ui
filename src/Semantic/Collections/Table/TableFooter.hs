module Semantic.Collections.Table.TableFooter where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data TableFooter ms = TableFooter_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (TableFooter ms) where
    def = (G.to gdef) { as = Tfoot }

pattern TableFooter :: TableFooter ms -> View ms
pattern TableFooter tf = View tf 

instance Pure TableFooter ms where
    render TableFooter_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children

instance HasAsProp (TableFooter ms) where
    type AsProp (TableFooter ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tf = tf { as = a }

instance HasAttributesProp (TableFooter ms) where
    type Attribute (TableFooter ms) = Feature ms
    getAttributes = attributes
    setAttributes as tf = tf { attributes = as }

instance HasChildrenProp (TableFooter ms) where
    type Child (TableFooter ms) = View ms
    getChildren = children
    setChildren cs tf = tf { children = cs }

instance HasClassesProp (TableFooter ms) where
    getClasses = classes
    setClasses cs tf = tf { classes = cs }

    