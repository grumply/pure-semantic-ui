module Semantic.Addons.Accordion where

import GHC.Generics as G
import Pure.View hiding (styled)

import Semantic.Utils

data Accordion ms = Accordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fluid :: Bool
    , inverted :: Bool
    , styled :: Bool
    } deriving (Generic)

instance Default (Accordion ms) where
    def = (G.to gdef) { as = Div }

pattern Accordion :: Typeable ms => Accordion ms -> View ms
pattern Accordion a = View a

instance Typeable ms => Pure Accordion ms where
    render Accordion_ {..} =
        let
            cs =
                ( "ui"
                : "accordion"
                : fluid # "fluid"
                : inverted # "inverted"
                : styled # "styled"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
