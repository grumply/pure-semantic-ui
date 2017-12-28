module Semantic.Collections.Breadcrumb (module Semantic.Collections.Breadcrumb, module Export) where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Collections.Breadcrumb.BreadcrumbDivider as Export
import Semantic.Collections.Breadcrumb.BreadcrumbSection as Export

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