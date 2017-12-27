module Semantic.Elements.Icon.IconGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data IconGroup ms = IconGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , children :: [View ms]
    , classes :: [Txt]
    , attributes :: [Feature ms]
    , size :: Txt
    } deriving (Generic)

instance Default (IconGroup ms) where
    def = (G.to gdef) { as = I }

pattern IconGroup :: Typeable ms => IconGroup ms -> View ms
pattern IconGroup ig = View ig

instance Typeable ms => Pure IconGroup ms where
    render IconGroup_ {..} =
        let
            cs =
                ( size
                : "icons"
                : classes
                )
        in as (ClassList cs : attributes) children

instance HasAs (IconGroup ms) where
    type Constructor (IconGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ig = ig { as = f }

instance HasAttributes (IconGroup ms) where
    type Attribute (IconGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ig = ig { attributes = cs }

instance HasChildren (IconGroup ms) where
    type Child (IconGroup ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClasses (IconGroup ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }