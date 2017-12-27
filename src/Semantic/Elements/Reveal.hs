module Semantic.Elements.Reveal where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data Reveal ms = Reveal_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , animated :: Txt
    , disabled :: Bool
    , instant :: Bool
    } deriving (Generic)

instance Default (Reveal ms) where
    def = (G.to gdef) { as = Div }

pattern Reveal :: Typeable ms => Reveal ms -> View ms
pattern Reveal r = View r

instance Typeable ms => Pure Reveal ms where
    render Reveal_ {..} =
        let
            cs =
                ( "ui"
                : animated
                : active # "active"
                : disabled # "disabled"
                : instant # "instant"
                : "reveal"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAs (Reveal ms) where
    type Constructor (Reveal ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f r = r { as = f }

instance HasAttributes (Reveal ms) where
    type Attribute (Reveal ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs r = r { attributes = cs }

instance HasChildren (Reveal ms) where
    type Child (Reveal ms) = View ms
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasClasses (Reveal ms) where
    getClasses = classes
    setClasses cs r = r { classes = cs }