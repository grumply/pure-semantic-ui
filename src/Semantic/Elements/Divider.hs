module Semantic.Elements.Divider where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

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