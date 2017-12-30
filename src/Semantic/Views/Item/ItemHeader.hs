module Semantic.Views.Item.ItemHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ItemHeader ms = ItemHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ItemHeader :: Typeable ms => ItemHeader ms -> View ms
pattern ItemHeader ih = View ih

instance Typeable ms => Pure ItemHeader ms where
    render ItemHeader_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
