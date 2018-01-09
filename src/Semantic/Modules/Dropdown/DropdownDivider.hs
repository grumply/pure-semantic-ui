module Semantic.Modules.Dropdown.DropdownDivider where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes

data DropdownDivider ms = DropdownDivider_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , classes :: [Txt]
    } deriving (Generic)

instance Default (DropdownDivider ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownDivider :: DropdownDivider ms -> View ms
pattern DropdownDivider dd = View dd

instance Pure DropdownDivider ms where
    render DropdownDivider_ {..} =
        let
            cs = 
                ( "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                []

instance HasAsProp (DropdownDivider ms) where
    type AsProp (DropdownDivider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dd = dd { as = f }

instance HasAttributesProp (DropdownDivider ms) where
    type Attribute (DropdownDivider ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs dd = dd { attributes = cs }

instance HasClassesProp (DropdownDivider ms) where
    getClasses = classes
    setClasses cs dd = dd { classes = cs }