module Semantic.Elements.Image.ImageGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Size

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

instance HasAsProp (ImageGroup ms) where
    type AsProp (ImageGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ig = ig { as = f }

instance HasAttributesProp (ImageGroup ms) where
    type Attribute (ImageGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ig = ig { attributes = cs }

instance HasChildrenProp (ImageGroup ms) where
    type Child (ImageGroup ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClassesProp (ImageGroup ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }

instance HasSizeProp (ImageGroup ms) where
    getSize = size
    setSize s ig = ig { size = s }