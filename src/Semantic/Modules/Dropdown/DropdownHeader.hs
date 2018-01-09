module Semantic.Modules.Dropdown.DropdownHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data DropdownHeader ms = DropdownHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (DropdownHeader ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownHeader :: DropdownHeader ms -> View ms
pattern DropdownHeader dh = View dh

instance Pure DropdownHeader ms where
    render DropdownHeader_ {..} =
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

instance HasAsProp (DropdownHeader ms) where
    type AsProp (DropdownHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dh = dh { as = f }

instance HasAttributesProp (DropdownHeader ms) where
    type Attribute (DropdownHeader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs dh = dh { attributes = cs }

instance HasChildrenProp (DropdownHeader ms) where
    type Child (DropdownHeader ms) = View ms
    getChildren = children
    setChildren cs dh = dh { children = cs }

instance HasClassesProp (DropdownHeader ms) where
    getClasses = classes
    setClasses cs dh = dh { classes = cs }