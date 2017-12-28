module Semantic.Collections.Form.FormGroup where

import GHC.Generics as G
import Pure.View hiding (disabled,inline,widths)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Grouped
import Semantic.Properties.Inline
import Semantic.Properties.Unstackable
import Semantic.Properties.Widths

data FormGroup ms = FormGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , grouped :: Bool
    , inline :: Bool
    , unstackable :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (FormGroup ms) where
    def = (G.to gdef) { as = Div }

pattern FormGroup :: Typeable ms => FormGroup ms -> View ms
pattern FormGroup fg = View fg

instance Typeable ms => Pure FormGroup ms where
    render FormGroup_ {..} =
        let
            cs =
                ( grouped # "grouped"
                : inline # "inline"
                : unstackable # "unstackable"
                : widthProp widths def True
                : "fields"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (FormGroup ms) where
    type AsProp (FormGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fg = fg { as = a }

instance HasAttributesProp (FormGroup ms) where
    type Attribute (FormGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as fg = fg { attributes = as }

instance HasChildrenProp (FormGroup ms) where
    type Child (FormGroup ms) = View ms
    getChildren = children
    setChildren cs fg = fg { children = cs }

instance HasClassesProp (FormGroup ms) where
    getClasses = classes
    setClasses cs fg = fg { classes = cs }

instance HasGroupedProp (FormGroup ms) where
    getGrouped = grouped
    setGrouped g fg = fg { grouped = g }

instance HasInlineProp (FormGroup ms) where
    type InlineProp (FormGroup ms) = Bool
    getInline = inline
    setInline i fg = fg { inline = i }

instance HasUnstackableProp (FormGroup ms) where
    getUnstackable = unstackable
    setUnstackable u fg = fg { unstackable = u }

instance HasWidthsProp (FormGroup ms) where
    getWidths = widths
    setWidths w fg = fg { widths = w }