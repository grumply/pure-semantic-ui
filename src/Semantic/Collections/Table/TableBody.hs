module Semantic.Collections.Table.TableBody where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data TableBody ms = TableBody_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (TableBody ms) where
    def = (G.to gdef) { as = Tbody }

pattern TableBody :: TableBody ms -> View ms
pattern TableBody tb = View tb

instance Pure TableBody ms where
    render TableBody_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children

instance HasAsProp (TableBody ms) where
    type AsProp (TableBody ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tb = tb { as = a }

instance HasAttributesProp (TableBody ms) where
    type Attribute (TableBody ms) = Feature ms
    getAttributes = attributes
    setAttributes as tb = tb { attributes = as }

instance HasChildrenProp (TableBody ms) where
    type Child (TableBody ms) = View ms
    getChildren = children
    setChildren cs tb = tb { children = cs }

instance HasClassesProp (TableBody ms) where
    getClasses = classes
    setClasses cs tb = tb { classes = cs }

    