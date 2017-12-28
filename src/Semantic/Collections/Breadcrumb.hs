module Semantic.Collections.Breadcrumb (module Semantic.Collections.Breadcrumb, module Export) where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Collections.Breadcrumb.BreadcrumbDivider as Export
import Semantic.Collections.Breadcrumb.BreadcrumbSection as Export

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Size

data Breadcrumb ms = Breadcrumb_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Breadcrumb ms) where
    def = (G.to gdef) { as = Div }

pattern Breadcrumb :: Typeable ms => Breadcrumb ms -> View ms
pattern Breadcrumb bc = View bc

instance Typeable ms => Pure Breadcrumb ms where
    render Breadcrumb_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "breadcrumb"
                : classes
                )
        in
            as 
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (Breadcrumb ms) where
    type AsProp (Breadcrumb ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bc = bc { as = a }

instance HasAttributesProp (Breadcrumb ms) where
    type Attribute (Breadcrumb ms) = Feature ms
    getAttributes = attributes
    setAttributes as bc = bc { attributes = as }

instance HasChildrenProp (Breadcrumb ms) where
    type Child (Breadcrumb ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasClassesProp (Breadcrumb ms) where
    getClasses = classes
    setClasses cs bc = bc { classes = cs }

instance HasSizeProp (Breadcrumb ms) where
    getSize = size
    setSize sz bc = bc { size = sz }