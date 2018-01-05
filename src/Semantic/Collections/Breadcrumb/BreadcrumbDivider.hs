module Semantic.Collections.Breadcrumb.BreadcrumbDivider where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data BreadcrumbDivider ms = BreadcrumbDivider_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (BreadcrumbDivider ms) where
    def = (G.to gdef) { as = Div }

pattern BreadcrumbDivider :: Typeable ms => BreadcrumbDivider ms -> View ms
pattern BreadcrumbDivider bcd = View bcd

instance Typeable ms => Pure BreadcrumbDivider ms where
    render BreadcrumbDivider_ {..} =
        let
            cs =
                ( "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                (children ? children $ "/")

instance HasAsProp (BreadcrumbDivider ms) where
    type AsProp (BreadcrumbDivider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bcd = bcd { as = a }

instance HasAttributesProp (BreadcrumbDivider ms) where
    type Attribute (BreadcrumbDivider ms) = Feature ms
    getAttributes = attributes
    setAttributes as bcd = bcd { attributes = as }

instance HasChildrenProp (BreadcrumbDivider ms) where
    type Child (BreadcrumbDivider ms) = View ms
    getChildren = children
    setChildren cs bcd = bcd { children = cs }

instance HasClassesProp (BreadcrumbDivider ms) where
    getClasses = classes
    setClasses cs bcd = bcd { classes = cs }