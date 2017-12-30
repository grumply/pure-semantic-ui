module Semantic.Views.Item.ItemMeta where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ItemMeta ms = ItemMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemMeta ms) where
    def = (G.to gdef) { as = Div }

pattern ItemMeta :: Typeable ms => ItemMeta ms -> View ms
pattern ItemMeta im = View im

instance Typeable ms => Pure ItemMeta ms where
    render ItemMeta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
