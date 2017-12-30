module Semantic.Views.Item.ItemExtra where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ItemExtra ms = ItemExtra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemExtra ms) where
    def = (G.to gdef) { as = Div }

pattern ItemExtra :: Typeable ms => ItemExtra ms -> View ms
pattern ItemExtra ie = View ie

instance Typeable ms => Pure ItemExtra ms where
    render ItemExtra_ {..} =
        let
            cs =
                ( "extra"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
