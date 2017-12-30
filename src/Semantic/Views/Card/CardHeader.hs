module Semantic.Views.Card.CardHeader where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

data CardHeader ms = CardHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardHeader ms) where
    def = (G.to gdef) { as = Div }

pattern CardHeader :: Typeable ms => CardHeader ms -> View ms
pattern CardHeader ch = View ch 

instance Typeable ms => Pure CardHeader ms where
    render CardHeader_ {..} =
        let
            cs =
                ( textAlign
                : "header"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
