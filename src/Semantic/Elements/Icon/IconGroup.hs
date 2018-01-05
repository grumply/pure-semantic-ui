module Semantic.Elements.Icon.IconGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Size

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
        in as (mergeClasses $ ClassList cs : attributes) children

instance HasAsProp (IconGroup ms) where
    type AsProp (IconGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ig = ig { as = f }

instance HasAttributesProp (IconGroup ms) where
    type Attribute (IconGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ig = ig { attributes = cs }

instance HasChildrenProp (IconGroup ms) where
    type Child (IconGroup ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClassesProp (IconGroup ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }

instance HasSizeProp (IconGroup ms) where
    getSize = size
    setSize s ig = ig { size = s }