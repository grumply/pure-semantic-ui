module Semantic.Views.Card.CardGroup where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

data CardGroup ms = CardGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , doubling :: Bool
    , itemsPerRow :: Width
    , stackable :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardGroup ms) where
    def = (G.to gdef) { as = Div }

pattern CardGroup :: Typeable ms => CardGroup ms -> View ms
pattern CardGroup cg = View cg

instance Typeable ms => Pure CardGroup ms where
    render CardGroup_ {..} =
        let
            cs =
                ( "ui"
                : doubling # "doubling"
                : stackable # "stackable"
                : textAlign
                : widthProp def width def
                : "cards"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
    setTextAlign ta cc = cc { textAlign = ta }