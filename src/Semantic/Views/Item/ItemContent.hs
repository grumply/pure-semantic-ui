module Semantic.Views.Item.ItemContent where

import GHC.Generics as G
import Pure.View hiding (verticalAlign)

import Semantic.Utils

data ItemContent ms = ItemContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (ItemContent ms) where
    def = (G.to gdef) { as = Div }

pattern ItemContent :: Typeable ms => ItemContent ms -> View ms
pattern ItemContent ic = View ic

instance Typeable ms => Pure ItemContent ms where
    render ItemContent_ {..} =
        let
            cs =
                ( verticalAlign
                : "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
