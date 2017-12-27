module Semantic.Elements.Label.LabelGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data LabelGroup ms = LabelGroup_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (LabelGroup ms) where
    def = (G.to gdef) { as = Div }

pattern LabelGroup :: Typeable ms => LabelGroup ms -> View ms
pattern LabelGroup lg = View lg

instance Typeable ms => Pure LabelGroup ms where
    render LabelGroup_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : circular # "circular" 
                : tag # "tag"
                : "labels"
                : classes
                )
        in
            as 
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (LabelGroup ms) where
    type AsProp (LabelGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lg = lg { as = f }

instance HasAttributesProp (LabelGroup ms) where
    type Attribute (LabelGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs lg = lg { attributes = cs }

instance HasChildrenProp (LabelGroup ms) where
    type Child (LabelGroup ms) = View ms
    getChildren = children
    setChildren cs lg = lg { children = cs } 

instance HasClassesProp (LabelGroup ms) where
    getClasses = classes
    setClasses cs lg = lg { classes = cs }