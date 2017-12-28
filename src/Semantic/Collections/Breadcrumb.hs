module Semantic.Collections.Breadcrumb where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

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