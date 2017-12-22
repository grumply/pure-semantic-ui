module Semantic.Elements.Icon.IconGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

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
