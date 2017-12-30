module Semantic.Views.Item.ItemDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ItemDescription ms = ItemDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ItemDescription :: Typeable ms => ItemDescription ms -> View ms
pattern ItemDescription id = View id

instance Typeable ms => Pure ItemDescription ms where
    render ItemDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
