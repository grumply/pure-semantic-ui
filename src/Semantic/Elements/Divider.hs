module Semantic.Elements.Divider where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Clearing

data Divider ms = Divider_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , clearing :: Bool
    , fitted :: Bool
    , hidden :: Bool
    , horizontal :: Bool
    , inverted :: Bool
    , section :: Bool
    , vertical :: Bool
    } deriving (Generic)

instance Default (Divider ms) where
    def = (G.to gdef) { as = Div }

instance Typeable ms => Pure Divider ms where
    render Divider_ {..} =
        let
            cs =
                ( "ui"
                : clearing # "clearing"
                : fitted # "fitted"
                : hidden # "hidden"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : section # "section"
                : vertical # "verical"
                : "divider"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (Divider ms) where
    type AsProp (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f d = d { as = f }

instance HasAttributesProp (Divider ms) where
    type Attribute (Divider ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs d = d { attributes = cs }

instance HasChildrenProp (Divider ms) where
    type Child (Divider ms) = View ms
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasClassesProp (Divider ms) where
    getClasses = classes
    setClasses cs d = d { classes = cs }

instance HasClearingProp (Divider ms) where
    getClearing = clearing
    setClearing c d = d { clearing = c }