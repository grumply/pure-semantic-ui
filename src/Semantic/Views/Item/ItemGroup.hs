module Semantic.Views.Item.ItemGroup where

import GHC.Generics as G
import Pure.View

import Semantic.Utils
data ItemGroup ms = ItemGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , divided :: Bool
    , link :: Bool
    , relaxed :: Maybe Txt
    , unstackable :: Bool
    } deriving (Generic)

instance Default (ItemGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ItemGroup :: Typeable ms => ItemGroup ms -> View ms
pattern ItemGroup ig = View ig

instance Typeable ms => Pure ItemGroup ms where
    render ItemGroup_ {..} =
        let
            cs =
                ( "ui"
                : divided # "divided"
                : link # "link"
                : unstackable # "unstackable"
                : may (<>> "relaxed") relaxed
                : "items"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
