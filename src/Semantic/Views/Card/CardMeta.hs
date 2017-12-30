module Semantic.Views.Card.CardMeta where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

data CardMeta ms = CardMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardMeta ms) where
    def = (G.to gdef) { as = Div }

pattern CardMeta :: Typeable ms => CardMeta ms -> View ms
pattern CardMeta cm = View cm

instance Typeable ms => Pure CardMeta ms where
    render CardMeta_ {..} =
        let
            cs =
                ( textAlign
                : "meta"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
