module Semantic.Elements.Divider where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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

instance HasAs (Divider ms) where
    type Constructor (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f d = d { as = f }

instance HasAttributes (Divider ms) where
    type Attribute (Divider ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs d = d { attributes = cs }

instance HasChildren (Divider ms) where
    type Child (Divider ms) = View ms
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasClasses (Divider ms) where
    getClasses = classes
    setClasses cs d = d { classes = cs }