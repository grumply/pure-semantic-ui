module Semantic.Elements.Image.ImageGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data ImageGroup ms = ImageGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (ImageGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ImageGroup :: Typeable ms => ImageGroup ms -> View ms
pattern ImageGroup ig = View ig

instance Typeable ms => Pure ImageGroup ms where
    render ImageGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : classes
                ) ++ [ "images" ]
        in 
            as (ClassList cs : attributes) children

instance HasAs (ImageGroup ms) where
    type Constructor (ImageGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ig = ig { as = f }

instance HasAttributes (ImageGroup ms) where
    type Attribute (ImageGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ig = ig { attributes = cs }

instance HasChildren (ImageGroup ms) where
    type Child (ImageGroup ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClasses (ImageGroup ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }