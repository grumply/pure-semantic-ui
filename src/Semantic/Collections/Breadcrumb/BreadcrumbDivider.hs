module Semantic.Collections.Breadcrumb.BreadcrumbDivider where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

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
                ( ClassList cs
                : attributes
                )
                children