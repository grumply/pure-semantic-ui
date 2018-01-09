module Semantic.Modules.Popup.PopupContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data PopupContent ms = PopupContent_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (PopupContent ms) where
    def = (G.to gdef) { as = Div }

pattern PopupContent :: PopupContent ms -> View ms
pattern PopupContent pc = View pc

instance Pure PopupContent ms where
    render PopupContent_ {..} =
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

instance HasAsProp (PopupContent ms) where
    type AsProp (PopupContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f pc = pc { as = f }

instance HasAttributesProp (PopupContent ms) where
    type Attribute (PopupContent ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs pc = pc { attributes = cs }

instance HasChildrenProp (PopupContent ms) where
    type Child (PopupContent ms) = View ms
    getChildren = children
    setChildren cs pc = pc { children = cs }

instance HasClassesProp (PopupContent ms) where
    getClasses = classes
    setClasses cs pc = pc { classes = cs }